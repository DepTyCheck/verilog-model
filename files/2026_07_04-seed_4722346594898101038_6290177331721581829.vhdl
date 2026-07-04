-- Seed: 4722346594898101038,6290177331721581829

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity iss is
  port (rm : linkage std_logic_vector(2 to 3); vknsr : buffer integer_vector(2 to 1); iqbeijtk : inout access_value_mirror);
end iss;

architecture mhj of iss is
  
begin
  -- Single-driven assignments
  vknsr <= (others => 0);
end mhj;

use std.reflection.all;

entity ldorslrufg is
  port (hnkl : inout floating_value_mirror; zicv : buffer time);
end ldorslrufg;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture vfsznhtoq of ldorslrufg is
  shared variable rkuqznk : access_value_mirror;
  signal zqbepk : integer_vector(2 to 1);
  signal nd : std_logic_vector(2 to 3);
  shared variable rhl : access_value_mirror;
  signal rqwwiyhk : integer_vector(2 to 1);
  signal qafbwt : std_logic_vector(2 to 3);
  shared variable fdfgxxu : access_value_mirror;
  signal kww : integer_vector(2 to 1);
  shared variable xqznrsp : access_value_mirror;
  signal plvlr : integer_vector(2 to 1);
  signal nxwnluk : std_logic_vector(2 to 3);
begin
  xdiw : entity work.iss
    port map (rm => nxwnluk, vknsr => plvlr, iqbeijtk => xqznrsp);
  vtfk : entity work.iss
    port map (rm => nxwnluk, vknsr => kww, iqbeijtk => fdfgxxu);
  eazh : entity work.iss
    port map (rm => qafbwt, vknsr => rqwwiyhk, iqbeijtk => rhl);
  ybbyprz : entity work.iss
    port map (rm => nd, vknsr => zqbepk, iqbeijtk => rkuqznk);
  
  -- Single-driven assignments
  zicv <= 20012.3 us;
  
  -- Multi-driven assignments
  nd <= "WX";
end vfsznhtoq;

use std.reflection.all;

entity toauryggm is
  port (k : buffer integer; cs : linkage real; y : inout record_value_mirror; dwqnqilox : out severity_level);
end toauryggm;

architecture ruhpmg of toauryggm is
  
begin
  -- Single-driven assignments
  dwqnqilox <= dwqnqilox;
  k <= 2#1_0_1_1_1#;
end ruhpmg;

use std.reflection.all;

entity ijs is
  port (ompogb : inout real_vector(0 to 3); dgqejrc : inout file_value_mirror);
end ijs;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture zqiahpfp of ijs is
  signal eufa : severity_level;
  shared variable vjzicpzrg : record_value_mirror;
  signal xvrnigl : real;
  signal jmdhkulh : integer;
  shared variable qtcvdzz : access_value_mirror;
  signal ltdxngysym : integer_vector(2 to 1);
  signal hkbkoscfh : std_logic_vector(2 to 3);
  signal hkvvr : severity_level;
  shared variable ufgyp : record_value_mirror;
  signal x : real;
  signal rn : integer;
  signal s : time;
  shared variable obuqjs : floating_value_mirror;
begin
  evmvsacu : entity work.ldorslrufg
    port map (hnkl => obuqjs, zicv => s);
  ensjwpck : entity work.toauryggm
    port map (k => rn, cs => x, y => ufgyp, dwqnqilox => hkvvr);
  oixtzhr : entity work.iss
    port map (rm => hkbkoscfh, vknsr => ltdxngysym, iqbeijtk => qtcvdzz);
  fvy : entity work.toauryggm
    port map (k => jmdhkulh, cs => xvrnigl, y => vjzicpzrg, dwqnqilox => eufa);
  
  -- Multi-driven assignments
  hkbkoscfh <= ('L', '0');
  hkbkoscfh <= hkbkoscfh;
end zqiahpfp;



-- Seed after: 3725322024355256996,6290177331721581829
