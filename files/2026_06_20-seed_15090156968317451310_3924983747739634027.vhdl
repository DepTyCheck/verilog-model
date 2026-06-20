-- Seed: 15090156968317451310,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity gkomndagl is
  port (abeyrmr : in boolean; lpl : inout boolean_vector(4 to 4); dgqjyla : inout std_logic_vector(2 downto 4); nugzltxh : inout integer);
end gkomndagl;

architecture qmcibphah of gkomndagl is
  
begin
  -- Single-driven assignments
  nugzltxh <= 2#1_1_0_0#;
  lpl <= (others => FALSE);
  
  -- Multi-driven assignments
  dgqjyla <= "";
end qmcibphah;

entity nk is
  port (ykgtlhtcwo : in real; upaqvm : inout boolean_vector(0 to 3));
end nk;

library ieee;
use ieee.std_logic_1164.all;

architecture xo of nk is
  signal lyrrqqibv : integer;
  signal xc : boolean_vector(4 to 4);
  signal l : integer;
  signal ipuhhfptld : std_logic_vector(2 downto 4);
  signal jllpwd : boolean_vector(4 to 4);
  signal rb : integer;
  signal pyrd : std_logic_vector(2 downto 4);
  signal ucvdtnapr : boolean_vector(4 to 4);
  signal gmh : boolean;
begin
  pb : entity work.gkomndagl
    port map (abeyrmr => gmh, lpl => ucvdtnapr, dgqjyla => pyrd, nugzltxh => rb);
  mkfri : entity work.gkomndagl
    port map (abeyrmr => gmh, lpl => jllpwd, dgqjyla => ipuhhfptld, nugzltxh => l);
  pzoyq : entity work.gkomndagl
    port map (abeyrmr => gmh, lpl => xc, dgqjyla => pyrd, nugzltxh => lyrrqqibv);
  
  -- Single-driven assignments
  gmh <= FALSE;
  upaqvm <= (TRUE, FALSE, TRUE, FALSE);
  
  -- Multi-driven assignments
  pyrd <= (others => '0');
end xo;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (dhvng : in std_logic_vector(0 downto 1); pupbtxhtbq : buffer time);
end d;

library ieee;
use ieee.std_logic_1164.all;

architecture kgzxrkl of d is
  signal ocmfqca : boolean_vector(0 to 3);
  signal o : real;
  signal zyg : integer;
  signal ijsjah : std_logic_vector(2 downto 4);
  signal poltivzy : boolean_vector(4 to 4);
  signal uj : boolean;
begin
  znach : entity work.gkomndagl
    port map (abeyrmr => uj, lpl => poltivzy, dgqjyla => ijsjah, nugzltxh => zyg);
  onlui : entity work.nk
    port map (ykgtlhtcwo => o, upaqvm => ocmfqca);
  
  -- Single-driven assignments
  pupbtxhtbq <= 00422 fs;
  
  -- Multi-driven assignments
  ijsjah <= "";
  ijsjah <= "";
  ijsjah <= "";
  ijsjah <= (others => '0');
end kgzxrkl;



-- Seed after: 3469483028087420669,3924983747739634027
