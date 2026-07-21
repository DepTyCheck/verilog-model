-- Seed: 16833860709052056688,11481034001933599325

entity znkhtq is
  port (lrpxdqv : in time_vector(3 downto 0));
end znkhtq;

architecture sydgyzm of znkhtq is
  
begin
  
end sydgyzm;

entity lxl is
  port (gigoledvbo : out time; ignyatu : out real_vector(1 to 2));
end lxl;

architecture nxuek of lxl is
  signal nm : time_vector(3 downto 0);
  signal ihqcztr : time_vector(3 downto 0);
begin
  pqidxcdz : entity work.znkhtq
    port map (lrpxdqv => ihqcztr);
  jl : entity work.znkhtq
    port map (lrpxdqv => nm);
  mkxspfjs : entity work.znkhtq
    port map (lrpxdqv => ihqcztr);
  
  -- Single-driven assignments
  ignyatu <= (16#4E35.8_A_D_F_5#, 42.3_2_1_3_2);
  gigoledvbo <= gigoledvbo;
end nxuek;

library ieee;
use ieee.std_logic_1164.all;

entity qclpbircl is
  port (cog : buffer std_logic_vector(2 to 0); mdg : inout integer; soha : in string(5 to 4); ef : inout integer);
end qclpbircl;

architecture aohvngez of qclpbircl is
  signal cjeu : time_vector(3 downto 0);
  signal otym : time_vector(3 downto 0);
begin
  r : entity work.znkhtq
    port map (lrpxdqv => otym);
  tsyp : entity work.znkhtq
    port map (lrpxdqv => otym);
  pmwgpl : entity work.znkhtq
    port map (lrpxdqv => cjeu);
  
  -- Single-driven assignments
  cjeu <= otym;
  otym <= otym;
  ef <= ef;
  
  -- Multi-driven assignments
  cog <= cog;
end aohvngez;

entity bjzsbdf is
  port (jnpeuqb : out integer);
end bjzsbdf;

library ieee;
use ieee.std_logic_1164.all;

architecture q of bjzsbdf is
  signal cfmfdxv : time_vector(3 downto 0);
  signal s : integer;
  signal duxwb : string(5 to 4);
  signal hwgnx : std_logic_vector(2 to 0);
  signal yebjl : real_vector(1 to 2);
  signal fb : time;
  signal r : time_vector(3 downto 0);
begin
  xj : entity work.znkhtq
    port map (lrpxdqv => r);
  lp : entity work.lxl
    port map (gigoledvbo => fb, ignyatu => yebjl);
  fuwtkke : entity work.qclpbircl
    port map (cog => hwgnx, mdg => jnpeuqb, soha => duxwb, ef => s);
  w : entity work.znkhtq
    port map (lrpxdqv => cfmfdxv);
  
  -- Multi-driven assignments
  hwgnx <= "";
  hwgnx <= "";
  hwgnx <= hwgnx;
  hwgnx <= hwgnx;
end q;



-- Seed after: 10314406567978049837,11481034001933599325
