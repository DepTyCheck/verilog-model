-- Seed: 10592367181264918511,11387579217500963635

library ieee;
use ieee.std_logic_1164.all;

entity essk is
  port (r : in integer; v : inout std_logic; jdoplqhbgy : in integer; tpngx : inout real_vector(4 downto 4));
end essk;



architecture iupliscvnl of essk is
  
begin
  
end iupliscvnl;

library ieee;
use ieee.std_logic_1164.all;

entity plubgo is
  port (znaphmlvh : linkage std_logic; izkmkob : inout bit; dr : in time);
end plubgo;

library ieee;
use ieee.std_logic_1164.all;

architecture jvwdsg of plubgo is
  signal yvevne : real_vector(4 downto 4);
  signal brpue : real_vector(4 downto 4);
  signal zvuirv : integer;
  signal rfxnaxy : std_logic;
  signal hfpc : integer;
begin
  wuinazgr : entity work.essk
    port map (r => hfpc, v => rfxnaxy, jdoplqhbgy => zvuirv, tpngx => brpue);
  pklfetxwz : entity work.essk
    port map (r => zvuirv, v => rfxnaxy, jdoplqhbgy => hfpc, tpngx => yvevne);
end jvwdsg;



entity jfunat is
  port (df : buffer integer; zgepxbha : buffer real; x : buffer integer);
end jfunat;

library ieee;
use ieee.std_logic_1164.all;

architecture dpfionnun of jfunat is
  signal jwialq : time;
  signal exajnch : bit;
  signal ctvej : real_vector(4 downto 4);
  signal goyapnk : integer;
  signal cpyz : integer;
  signal d : real_vector(4 downto 4);
  signal wbnjztcsrb : integer;
  signal ycvq : real_vector(4 downto 4);
  signal cdfpgaydcm : integer;
  signal jdxuk : std_logic;
begin
  eeyrz : entity work.essk
    port map (r => x, v => jdxuk, jdoplqhbgy => cdfpgaydcm, tpngx => ycvq);
  tnrzhrav : entity work.essk
    port map (r => x, v => jdxuk, jdoplqhbgy => wbnjztcsrb, tpngx => d);
  ffhgd : entity work.essk
    port map (r => cpyz, v => jdxuk, jdoplqhbgy => goyapnk, tpngx => ctvej);
  iel : entity work.plubgo
    port map (znaphmlvh => jdxuk, izkmkob => exajnch, dr => jwialq);
end dpfionnun;

library ieee;
use ieee.std_logic_1164.all;

entity fvet is
  port (rfprwkrvse : buffer real; vloxo : in boolean_vector(3 downto 1); rxky : buffer character; uzzzxai : inout std_logic_vector(2 to 1));
end fvet;

library ieee;
use ieee.std_logic_1164.all;

architecture ffol of fvet is
  signal t : bit;
  signal xwv : time;
  signal om : bit;
  signal e : std_logic;
  signal utrcpkt : time;
  signal bdhsxlnn : bit;
  signal ctcxqt : std_logic;
begin
  zrii : entity work.plubgo
    port map (znaphmlvh => ctcxqt, izkmkob => bdhsxlnn, dr => utrcpkt);
  hqsyt : entity work.plubgo
    port map (znaphmlvh => e, izkmkob => om, dr => xwv);
  gkygfj : entity work.plubgo
    port map (znaphmlvh => ctcxqt, izkmkob => t, dr => utrcpkt);
end ffol;



-- Seed after: 3653660270073820000,11387579217500963635
