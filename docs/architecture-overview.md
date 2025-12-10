# Erlang Sense HAT Architecture

## System Overview

```mermaid
flowchart TB
    subgraph "User Application"
        App[Erlang Application]
    end
    
    subgraph "LED Display System"
        SH[sensehat.erl<br/>Main API]
        FB[shfb.erl<br/>Framebuffer]
        EX[shexample.erl<br/>Examples]
    end
    
    subgraph "Joystick System"
        SUP[st_supervisor.erl<br/>Supervisor]
        MGR[st_event_manager.erl<br/>Event Manager]
        PORT[st_port.erl<br/>Port GenServer]
        EVT[st_event.erl<br/>Event Handler]
    end
    
    subgraph "Port Drivers"
        SHDRV[sensehat_drv.c<br/>LED Driver]
        STDRV[sensestick_drv.c<br/>Joystick Driver]
    end
    
    subgraph "Hardware"
        LED[8x8 LED Matrix<br/>/dev/fb1]
        JOY[5-Button Joystick<br/>/dev/input/event*]
    end
    
    App --> SH
    App --> EX
    App --> SUP
    
    SH --> FB
    EX --> SH
    
    SH --> SHDRV
    FB -.data.-> SH
    
    SUP --> MGR
    SUP --> PORT
    PORT --> STDRV
    MGR --> EVT
    EVT -.events.-> App
    
    SHDRV --> LED
    STDRV --> JOY
    
    style SH fill:#e1f5ff
    style SUP fill:#ffe1e1
    style SHDRV fill:#e1ffe1
    style STDRV fill:#e1ffe1
    style LED fill:#fff9e1
    style JOY fill:#fff9e1
```

## LED Display Architecture

```mermaid
flowchart LR
    subgraph "API Layer"
        API["sensehat.erl
        set_pixel/3
        set_pixels/1
        clear/0
        set_rotation/1"]
    end
    
    subgraph "Business Logic"
        FB["shfb.erl
        Framebuffer
        Rotation
        RGB Encoding"]
    end
    
    subgraph "Port Communication"
        PORT["Port Process
        Binary Protocol
        Command: 1"]
    end
    
    subgraph "C Driver"
        DRV["sensehat_drv.c
        mmap
        Framebuffer
        Hardware Access"]
    end
    
    subgraph "Linux Kernel"
        KERNEL["/dev/fb1
        Framebuffer Device
        RGB565 Format"]
    end
    
    API -->|State Updates| FB
    FB -->|Binary Data| PORT
    PORT -->|Binary Protocol| DRV
    DRV -->|mmap/write| KERNEL
    
    style API fill:#4fc3f7
    style FB fill:#81c784
    style PORT fill:#ffb74d
    style DRV fill:#e57373
    style KERNEL fill:#ba68c8
```

## Joystick Event Flow

```mermaid
sequenceDiagram
    participant HW as Hardware (Joystick)
    participant DRV as sensestick_drv.c
    participant PORT as st_port.erl
    participant MGR as st_event_manager
    participant EVT as st_event.erl
    participant APP as User Application
    
    Note over HW,DRV: Hardware Event
    HW->>DRV: Button Press
    DRV->>DRV: Convert to Code (1-5)
    
    Note over DRV,PORT: Port Communication
    DRV->>PORT: {data, [Code]}
    PORT->>PORT: code_to_event/1
    
    Note over PORT,MGR: Event Publishing
    PORT->>MGR: notify(Event)
    MGR->>EVT: handle_event(Event, Pid)
    
    Note over EVT,APP: Event Delivery
    EVT->>APP: {st_event, Event}
    
    Note over APP: Application handles: up, down, left, right, enter
```

## Process Supervision Tree

```mermaid
flowchart TB
    subgraph "OTP Supervision"
        SUP["st_supervisor
        Strategy: one_for_one
        Intensity: 5/60s"]
    end
    
    subgraph "Event System"
        MGR["st_event_manager
        Type: gen_event
        Restart: permanent"]
        
        subgraph "Dynamic Handlers"
            H1["st_event Handler 1"]
            H2["st_event Handler 2"]
            H3["st_event Handler N"]
        end
    end
    
    subgraph "Port Driver"
        PORT["st_port
        Type: gen_server
        Restart: permanent"]
        DRV["Port sensestick_drv"]
    end
    
    SUP -->|supervises| MGR
    SUP -->|supervises| PORT
    
    MGR -.dynamically manages.-> H1
    MGR -.dynamically manages.-> H2
    MGR -.dynamically manages.-> H3
    
    PORT -->|owns| DRV
    
    H1 -.sends events to.-> APP1[App Process 1]
    H2 -.sends events to.-> APP2[App Process 2]
    H3 -.sends events to.-> APP3[App Process N]
    
    style SUP fill:#ffcdd2
    style MGR fill:#c5e1a5
    style PORT fill:#b3e5fc
    style H1 fill:#f0f4c3
    style H2 fill:#f0f4c3
    style H3 fill:#f0f4c3
```

## Data Flow - Setting a Pixel

```mermaid
flowchart TD
    Start(["Application calls set_pixel/3"]) --> Check{"Process Running?"}
    
    Check -->|No| Error["Return error: not_running"]
    Check -->|Yes| Send["Send message to sensehat process"]
    
    Send --> Loop["loop/2 receives set_pixel message"]
    
    Loop --> Update["shfb:set_pixel/4 Updates framebuffer"]
    
    Update --> Render["shfb:to_binary/1 Apply rotation Convert to binary"]
    
    Render --> Port["Send to port Command 1 with Binary data"]
    
    Port --> Driver["C Driver receives binary framebuffer"]
    
    Driver --> Write["mmap write to /dev/fb1"]
    
    Write --> Display["LED Matrix displays pixel"]
    
    Display --> NextLoop["Loop waiting for next message"]
    
    style Start fill:#81c784
    style Check fill:#ffb74d
    style Error fill:#e57373
    style Display fill:#4fc3f7
    style NextLoop fill:#ba68c8
```

## Component Relationships

```mermaid
graph TB
    subgraph "Public API"
        sensehat
        shexample
        st_supervisor
        st_event_manager
    end
    
    subgraph "Internal Modules"
        shfb
        st_port
        st_event
    end
    
    subgraph "Port Drivers"
        sensehat_drv[sensehat_drv.c]
        sensestick_drv[sensestick_drv.c]
    end
    
    subgraph "System"
        fb1["/dev/fb1 Framebuffer"]
        input["/dev/input/event* Input Device"]
    end
    
    sensehat -->|uses| shfb
    sensehat -->|loads| sensehat_drv
    shexample -->|uses| sensehat
    
    st_supervisor -->|starts| st_event_manager
    st_supervisor -->|starts| st_port
    st_port -->|loads| sensestick_drv
    st_port -->|notifies| st_event_manager
    st_event_manager -->|manages| st_event
    
    sensehat_drv -->|mmap| fb1
    sensestick_drv -->|read| input
    
    style sensehat fill:#e3f2fd
    style st_supervisor fill:#fce4ec
    style sensehat_drv fill:#e8f5e9
    style sensestick_drv fill:#e8f5e9
```
