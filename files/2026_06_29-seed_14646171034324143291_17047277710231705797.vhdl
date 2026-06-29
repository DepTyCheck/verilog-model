-- Seed: 14646171034324143291,17047277710231705797

entity xgphma is
  port (aygykkz : in boolean_vector(0 to 0); xsqphij : buffer character);
end xgphma;

architecture ygbujgm of xgphma is
  
begin
  -- Single-driven assignments
  xsqphij <= 'n';
end ygbujgm;

entity jqx is
  port (jsjqtsodc : buffer real_vector(4 to 3); hlfn : inout integer);
end jqx;

architecture a of jqx is
  signal cleppq : character;
  signal cniun : boolean_vector(0 to 0);
begin
  g : entity work.xgphma
    port map (aygykkz => cniun, xsqphij => cleppq);
  
  -- Single-driven assignments
  jsjqtsodc <= (others => 0.0);
  cniun <= (others => TRUE);
  hlfn <= 2#1_0_0#;
end a;

library ieee;
use ieee.std_logic_1164.all;

entity zvqmlf is
  port (e : linkage std_logic_vector(4 downto 1); gdd : buffer integer; tvrijukci : out real);
end zvqmlf;

architecture ukikz of zvqmlf is
  signal zyixieogce : real_vector(4 to 3);
  signal zpn : character;
  signal vlbhwwd : boolean_vector(0 to 0);
begin
  q : entity work.xgphma
    port map (aygykkz => vlbhwwd, xsqphij => zpn);
  ajxmvkq : entity work.jqx
    port map (jsjqtsodc => zyixieogce, hlfn => gdd);
end ukikz;

entity yaelux is
  port (fmzu : linkage bit; gh : inout boolean);
end yaelux;

library ieee;
use ieee.std_logic_1164.all;

architecture zsfzsxhq of yaelux is
  signal blopsdgy : real;
  signal pbiiskgc : integer;
  signal msvkawuoyq : std_logic_vector(4 downto 1);
  signal m : real;
  signal tbftofvs : integer;
  signal l : std_logic_vector(4 downto 1);
  signal ylbeo : character;
  signal kip : boolean_vector(0 to 0);
  signal xzjla : integer;
  signal hhyyu : real_vector(4 to 3);
begin
  wsk : entity work.jqx
    port map (jsjqtsodc => hhyyu, hlfn => xzjla);
  mesxorizct : entity work.xgphma
    port map (aygykkz => kip, xsqphij => ylbeo);
  yymrmcuwzq : entity work.zvqmlf
    port map (e => l, gdd => tbftofvs, tvrijukci => m);
  p : entity work.zvqmlf
    port map (e => msvkawuoyq, gdd => pbiiskgc, tvrijukci => blopsdgy);
  
  -- Single-driven assignments
  gh <= TRUE;
  kip <= (others => FALSE);
  
  -- Multi-driven assignments
  l <= "U-W-";
  l <= "LLH-";
  msvkawuoyq <= "LH-H";
  msvkawuoyq <= "0XU-";
end zsfzsxhq;



-- Seed after: 13763594349364538587,17047277710231705797
