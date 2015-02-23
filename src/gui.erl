-module(gui).
-author("Henry").

-compile([export_all, debug_info]).

-include_lib("wx/include/wx.hrl").

-define(ABOUT, ?wxID_ABOUT).
-define(EXIT, ?wxID_EXIT).

test() ->
  spawn_link(fun() -> starter() end).

starter() ->
  Server = wx:new(),
  Pixels = <<255, 0, 0,   0, 255, 0,
             0, 0, 255,   255, 255, 255>>,
  Image = wxImage:new(2, 2),
  wxImage:setData(Image, Pixels),
  Bitmap = wxBitmap:new(Pixels, 2, 2),
  Frame = wxFrame:new(Server, ?wxID_ANY, "Image"),
  PaintDC = wxPaintDC:new(Frame),
  wxDC:drawBitmap(PaintDC, Bitmap, {0, 0}),
  wxFrame:show(Frame),
  looper(),
  wx:destroy().

looper() ->
  receive
    stop ->
      ok
  end.


start() ->
  wx:new(),
  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "MicroBlog"),
  setup(Frame),
  wxFrame:show(Frame),
  loop(Frame),
  wx:destroy().

setup(Frame) ->
  MenuBar = wxMenuBar:new(),
  File = wxMenu:new(),
  Help = wxMenu:new(),

  wxMenu:append(Help, ?ABOUT, "About MicroBlog"),
  wxMenu:append(File, ?EXIT, "Quit"),

  wxMenuBar:append(MenuBar, File, "&File"),
  wxMenuBar:append(MenuBar, Help, "&Help"),

  wxFrame:setMenuBar(Frame, MenuBar),

  wxFrame:createStatusBar(Frame),
  wxFrame:setStatusText(Frame, "Welcome to wxErlang"),

  wxFrame:connect(Frame, command_menu_selected),
  wxFrame:connect(Frame, close_window).

loop(Frame) ->
  receive
    #wx{id = ?ABOUT, event = #wxCommand{}} ->
      Str = "MicroBlog is a minimal WxErlang example!",
      MD = wxMessageDialog:new(Frame, Str, [{style, ?wxOK bor ?wxICON_INFORMATION}, {caption, "About MicroBlog"}]),
      wxDialog:showModal(MD),
      wxDialog:destroy(MD),
      loop(Frame);
    #wx{id = ?EXIT, event = #wxCommand{type = command_menu_selected}} ->
      wxWindow:close(Frame)
  end.

