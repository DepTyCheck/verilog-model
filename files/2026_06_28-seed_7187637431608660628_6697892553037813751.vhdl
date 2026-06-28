-- Seed: 7187637431608660628,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity akegyzs is
  port (wegbk : inout std_logic_vector(2 downto 3); cevp : in bit; ojrlt : inout std_logic; onugr : in boolean_vector(4 downto 4));
end akegyzs;

architecture jkdgcwagog of akegyzs is
  
begin
  
end jkdgcwagog;

library ieee;
use ieee.std_logic_1164.all;

entity nxbynavin is
  port (vjmytsm : out std_logic_vector(1 to 1); fmzmavtbuw : inout std_logic_vector(2 downto 3); uzfeytyx : out severity_level);
end nxbynavin;

library ieee;
use ieee.std_logic_1164.all;

architecture qmz of nxbynavin is
  signal bjaff : bit;
  signal rpjjf : boolean_vector(4 downto 4);
  signal btkyueuk : std_logic;
  signal yzemiz : boolean_vector(4 downto 4);
  signal fexaqmbv : std_logic;
  signal bceeo : std_logic_vector(2 downto 3);
  signal egywdx : boolean_vector(4 downto 4);
  signal q : std_logic;
  signal mnshtx : bit;
  signal kjkaxx : std_logic_vector(2 downto 3);
begin
  ioxkxmxt : entity work.akegyzs
    port map (wegbk => kjkaxx, cevp => mnshtx, ojrlt => q, onugr => egywdx);
  kwghdkk : entity work.akegyzs
    port map (wegbk => bceeo, cevp => mnshtx, ojrlt => fexaqmbv, onugr => yzemiz);
  cdapdbhl : entity work.akegyzs
    port map (wegbk => bceeo, cevp => mnshtx, ojrlt => btkyueuk, onugr => rpjjf);
  dxfppmzq : entity work.akegyzs
    port map (wegbk => bceeo, cevp => bjaff, ojrlt => q, onugr => egywdx);
  
  -- Single-driven assignments
  yzemiz <= (others => TRUE);
  egywdx <= (others => TRUE);
  bjaff <= '0';
  uzfeytyx <= FAILURE;
  mnshtx <= '1';
end qmz;

entity vx is
  port (jbpqxp : inout boolean_vector(2 to 0));
end vx;

architecture vmyifoe of vx is
  
begin
  -- Single-driven assignments
  jbpqxp <= (others => TRUE);
end vmyifoe;



-- Seed after: 13839333082288012557,6697892553037813751
