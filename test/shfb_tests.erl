%%% @doc Unit tests for shfb module (framebuffer utilities)
%%% @end

-module(shfb_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% Test framebuffer creation
create_black_test() ->
    FB = shfb:create(0),
    Binary = shfb:to_binary(FB),
    ?assertEqual(192, byte_size(Binary)), % 8x8 pixels * 24 bits = 192 bytes
    % All pixels should be black (0x000000)
    ?assertEqual(<<0:(192*8)>>, Binary).

create_with_color_test() ->
    RGB = 16#FF0000, % Red (0xFF, 0x00, 0x00)
    FB = shfb:create(RGB),
    Binary = shfb:to_binary(FB),
    ?assertEqual(192, byte_size(Binary)),
    % Check that Binary contains red pixels
    ?assertNotEqual(<<0:(192*8)>>, Binary).

%% Test pixel setting (coordinates are 1-based in implementation)
set_pixel_test() ->
    FB = shfb:create(0),
    RGB = 16#FF0000, % Red
    FB2 = shfb:set_pixel(1, 1, RGB, FB),
    
    Binary = shfb:to_binary(FB2),
    ?assertEqual(192, byte_size(Binary)),
    
    % Verify the first pixel changed
    <<Pixel:24, _Rest/binary>> = Binary,
    ?assertEqual(RGB, Pixel).

set_pixel_bounds_test() ->
    FB = shfb:create(0),
    % Test all corners (1-based coordinates)
    FB1 = shfb:set_pixel(1, 1, 16#FF0000, FB),
    FB2 = shfb:set_pixel(1, 8, 16#00FF00, FB1),
    FB3 = shfb:set_pixel(8, 1, 16#0000FF, FB2),
    FB4 = shfb:set_pixel(8, 8, 16#FFFFFF, FB3),
    
    Binary = shfb:to_binary(FB4),
    ?assertEqual(192, byte_size(Binary)).

%% Test multiple pixel setting
set_pixels_test() ->
    FB = shfb:create(0),
    % set_pixels takes a full matrix of pixels (8x8)
    Pixels = [[16#FF0000, 0, 0, 0, 0, 0, 0, 0],
              [0, 16#00FF00, 0, 0, 0, 0, 0, 0],
              [0, 0, 16#0000FF, 0, 0, 0, 0, 0],
              [0, 0, 0, 0, 0, 0, 0, 0],
              [0, 0, 0, 0, 0, 0, 0, 0],
              [0, 0, 0, 0, 0, 0, 0, 0],
              [0, 0, 0, 0, 0, 0, 0, 0],
              [0, 0, 0, 0, 0, 0, 0, 0]],
    FB2 = shfb:set_pixels(Pixels, FB),
    
    Binary = shfb:to_binary(FB2),
    ?assertEqual(192, byte_size(Binary)),
    
    % Check first three pixels
    <<P1:24, P2:24, P3:24, _/binary>> = Binary,
    ?assertEqual(16#FF0000, P1),
    ?assertEqual(0, P2),
    ?assertEqual(0, P3).

%% Test rotation
set_rotation_0_test() ->
    FB = shfb:create(0),
    FB2 = shfb:set_pixel(1, 1, 16#FF0000, FB),
    FB3 = shfb:set_rotation(0, FB2),
    
    % Both should produce the same binary
    ?assertEqual(shfb:to_binary(FB2), shfb:to_binary(FB3)).

set_rotation_90_test() ->
    FB = shfb:create(0),
    FB2 = shfb:set_pixel(1, 1, 16#FF0000, FB),
    FB3 = shfb:set_rotation(90, FB2),
    
    Binary2 = shfb:to_binary(FB2),
    Binary3 = shfb:to_binary(FB3),
    ?assertEqual(192, byte_size(Binary3)),
    % Rotation should change the output
    ?assertNotEqual(Binary2, Binary3).

set_rotation_180_test() ->
    FB = shfb:create(0),
    FB2 = shfb:set_pixel(1, 1, 16#FF0000, FB),
    FB3 = shfb:set_rotation(180, FB2),
    
    Binary3 = shfb:to_binary(FB3),
    ?assertEqual(192, byte_size(Binary3)).

set_rotation_270_test() ->
    FB = shfb:create(0),
    FB2 = shfb:set_pixel(1, 1, 16#FF0000, FB),
    FB3 = shfb:set_rotation(270, FB2),
    
    Binary3 = shfb:to_binary(FB3),
    ?assertEqual(192, byte_size(Binary3)).

%% Test binary conversion
to_binary_test() ->
    FB = shfb:create(0),
    Binary = shfb:to_binary(FB),
    ?assertEqual(192, byte_size(Binary)),
    ?assertEqual(<<0:(192*8)>>, Binary).

to_binary_with_pixels_test() ->
    FB = shfb:create(0),
    RGB = 16#808080, % Gray
    FB2 = shfb:set_pixel(4, 4, RGB, FB),
    Binary = shfb:to_binary(FB2),
    ?assertEqual(192, byte_size(Binary)).
