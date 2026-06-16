-- Seed: 4208211697004713236,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity yyvr is
  port (eldckbl : inout real; zivarkfmrl : in std_logic_vector(3 downto 0); brvb : linkage std_logic; xllzxd : in std_logic);
end yyvr;

architecture vqcqbleq of yyvr is
  
begin
  -- Single-driven assignments
  eldckbl <= 16#7.B#;
end vqcqbleq;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (o : out std_logic_vector(4 to 2));
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture fe of f is
  signal sto : std_logic_vector(3 downto 0);
  signal xsnmbe : real;
  signal tihuwaxxtc : std_logic;
  signal wphe : std_logic_vector(3 downto 0);
  signal hs : real;
  signal cz : std_logic;
  signal iswdmzp : std_logic_vector(3 downto 0);
  signal jpjvp : real;
begin
  fbwwikdym : entity work.yyvr
    port map (eldckbl => jpjvp, zivarkfmrl => iswdmzp, brvb => cz, xllzxd => cz);
  kgn : entity work.yyvr
    port map (eldckbl => hs, zivarkfmrl => wphe, brvb => tihuwaxxtc, xllzxd => cz);
  qatmge : entity work.yyvr
    port map (eldckbl => xsnmbe, zivarkfmrl => sto, brvb => cz, xllzxd => cz);
  
  -- Multi-driven assignments
  o <= "";
  sto <= "-UZ1";
  tihuwaxxtc <= 'U';
  iswdmzp <= ('U', 'U', 'U', '-');
end fe;

entity ayqtvv is
  port (zrlv : out time; gqd : inout severity_level; ogsbl : out real_vector(2 downto 3));
end ayqtvv;

library ieee;
use ieee.std_logic_1164.all;

architecture zc of ayqtvv is
  signal falsqzhs : std_logic_vector(3 downto 0);
  signal irsdjn : real;
  signal hapqwr : std_logic_vector(4 to 2);
  signal zbwbcuowh : std_logic;
  signal rfzx : std_logic_vector(3 downto 0);
  signal kgfqcfs : real;
begin
  s : entity work.yyvr
    port map (eldckbl => kgfqcfs, zivarkfmrl => rfzx, brvb => zbwbcuowh, xllzxd => zbwbcuowh);
  dhdtvjtgp : entity work.f
    port map (o => hapqwr);
  vhscc : entity work.yyvr
    port map (eldckbl => irsdjn, zivarkfmrl => falsqzhs, brvb => zbwbcuowh, xllzxd => zbwbcuowh);
end zc;



-- Seed after: 13285150058461413429,5472058987609252853
