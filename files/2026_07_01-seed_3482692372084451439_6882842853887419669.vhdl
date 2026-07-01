-- Seed: 3482692372084451439,6882842853887419669

entity iw is
  port (a : in integer; dsnqdpyt : out integer; mr : inout integer_vector(3 downto 1); troxq : inout real);
end iw;

architecture jxhdzethj of iw is
  
begin
  -- Single-driven assignments
  troxq <= 333.021;
  dsnqdpyt <= 0141;
  mr <= (16#F_B_E_6_D#, 2#1011#, 3224);
end jxhdzethj;

library ieee;
use ieee.std_logic_1164.all;

entity vjpx is
  port (k : buffer std_logic; rcmmz : inout std_logic; kgp : inout real; jrpxr : buffer time);
end vjpx;

architecture xgnugc of vjpx is
  signal r : real;
  signal yochgsqxm : integer_vector(3 downto 1);
  signal rvvu : integer;
  signal qkxcej : real;
  signal pba : integer_vector(3 downto 1);
  signal ref : real;
  signal zmdybvdye : integer_vector(3 downto 1);
  signal jzvclzx : integer;
  signal fvj : integer;
  signal tnpv : integer_vector(3 downto 1);
  signal monowlakxr : integer;
  signal vgintpin : integer;
begin
  kieifwdo : entity work.iw
    port map (a => vgintpin, dsnqdpyt => monowlakxr, mr => tnpv, troxq => kgp);
  qwysof : entity work.iw
    port map (a => fvj, dsnqdpyt => jzvclzx, mr => zmdybvdye, troxq => ref);
  jxo : entity work.iw
    port map (a => vgintpin, dsnqdpyt => vgintpin, mr => pba, troxq => qkxcej);
  bldetrollb : entity work.iw
    port map (a => jzvclzx, dsnqdpyt => rvvu, mr => yochgsqxm, troxq => r);
  
  -- Single-driven assignments
  jrpxr <= 4_3_2_3.0_3_3 ms;
  
  -- Multi-driven assignments
  k <= '0';
  rcmmz <= 'X';
  rcmmz <= '1';
  rcmmz <= 'H';
end xgnugc;



-- Seed after: 10150415714657916917,6882842853887419669
