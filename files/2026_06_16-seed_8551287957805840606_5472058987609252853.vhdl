-- Seed: 8551287957805840606,5472058987609252853

entity llbbsh is
  port (u : out integer);
end llbbsh;

architecture ckmxyngqr of llbbsh is
  
begin
  -- Single-driven assignments
  u <= 16#A#;
end ckmxyngqr;

library ieee;
use ieee.std_logic_1164.all;

entity isy is
  port (kqrd : buffer std_logic_vector(4 to 1); kazzx : linkage std_logic; fmqmco : in integer; afqvftbjv : inout std_logic_vector(4 to 0));
end isy;

architecture uofbtbwbdx of isy is
  signal wth : integer;
  signal zlsbsi : integer;
  signal ajknaxyi : integer;
begin
  rqxoz : entity work.llbbsh
    port map (u => ajknaxyi);
  ihyscpbm : entity work.llbbsh
    port map (u => zlsbsi);
  nkuu : entity work.llbbsh
    port map (u => wth);
  
  -- Multi-driven assignments
  afqvftbjv <= "";
  kqrd <= "";
  afqvftbjv <= "";
  afqvftbjv <= "";
end uofbtbwbdx;

entity lewbx is
  port (vlvar : out integer);
end lewbx;

library ieee;
use ieee.std_logic_1164.all;

architecture caiafpg of lewbx is
  signal oy : std_logic_vector(4 to 0);
  signal eg : integer;
  signal avqnj : std_logic;
  signal hzy : std_logic_vector(4 to 1);
begin
  tckxwf : entity work.llbbsh
    port map (u => vlvar);
  funsnqqf : entity work.isy
    port map (kqrd => hzy, kazzx => avqnj, fmqmco => eg, afqvftbjv => oy);
  f : entity work.llbbsh
    port map (u => eg);
end caiafpg;

entity yt is
  port (wqflmg : inout time; vhe : buffer real);
end yt;

architecture qjn of yt is
  signal ruvjbu : integer;
  signal ojqecd : integer;
  signal qrgscuno : integer;
  signal krwdotutrk : integer;
begin
  ydilyrmp : entity work.llbbsh
    port map (u => krwdotutrk);
  p : entity work.llbbsh
    port map (u => qrgscuno);
  j : entity work.llbbsh
    port map (u => ojqecd);
  z : entity work.llbbsh
    port map (u => ruvjbu);
  
  -- Single-driven assignments
  wqflmg <= 3_4_0 fs;
  vhe <= 16#E21D6.B_D#;
end qjn;



-- Seed after: 17753371874118423251,5472058987609252853
