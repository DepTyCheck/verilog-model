-- Seed: 13910756011808828593,13501862637168280927

entity piw is
  port (enmp : in real_vector(3 downto 1); iimxlnv : buffer real; tjyfda : inout bit_vector(4 downto 3); srwhfzc : out boolean);
end piw;

architecture ow of piw is
  
begin
  -- Single-driven assignments
  srwhfzc <= TRUE;
  tjyfda <= ('1', '0');
  iimxlnv <= 2#1.1_1_1#;
end ow;

library ieee;
use ieee.std_logic_1164.all;

entity fvihuizpve is
  port (bvndeejs : in real; nkr : in std_logic; qhnu : inout std_logic_vector(1 to 2));
end fvihuizpve;

architecture ziswwebne of fvihuizpve is
  signal ql : boolean;
  signal nu : bit_vector(4 downto 3);
  signal t : real;
  signal zbulu : boolean;
  signal npijmr : bit_vector(4 downto 3);
  signal mqyro : real;
  signal ecaahjpii : real_vector(3 downto 1);
  signal dtl : boolean;
  signal bo : bit_vector(4 downto 3);
  signal jhtz : real;
  signal ajnbki : real_vector(3 downto 1);
  signal fdvnsy : boolean;
  signal wac : bit_vector(4 downto 3);
  signal lpre : real;
  signal ohkkbkk : real_vector(3 downto 1);
begin
  eabfpxmttu : entity work.piw
    port map (enmp => ohkkbkk, iimxlnv => lpre, tjyfda => wac, srwhfzc => fdvnsy);
  vklsmp : entity work.piw
    port map (enmp => ajnbki, iimxlnv => jhtz, tjyfda => bo, srwhfzc => dtl);
  lxmg : entity work.piw
    port map (enmp => ecaahjpii, iimxlnv => mqyro, tjyfda => npijmr, srwhfzc => zbulu);
  g : entity work.piw
    port map (enmp => ajnbki, iimxlnv => t, tjyfda => nu, srwhfzc => ql);
end ziswwebne;

entity arxlvxfy is
  port (x : linkage bit; gxc : buffer integer);
end arxlvxfy;

architecture vuwlgrsi of arxlvxfy is
  signal sagcueb : boolean;
  signal tpiws : bit_vector(4 downto 3);
  signal jqllpv : real;
  signal mzorrbz : real_vector(3 downto 1);
begin
  y : entity work.piw
    port map (enmp => mzorrbz, iimxlnv => jqllpv, tjyfda => tpiws, srwhfzc => sagcueb);
  
  -- Single-driven assignments
  gxc <= gxc;
  mzorrbz <= mzorrbz;
end vuwlgrsi;



-- Seed after: 15998841316963193708,13501862637168280927
