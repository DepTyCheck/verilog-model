-- Seed: 15961582943487172888,5472058987609252853

entity uccihv is
  port (zhw : buffer time; mjzqdsxzay : linkage severity_level);
end uccihv;

architecture vyjyggwct of uccihv is
  
begin
  -- Single-driven assignments
  zhw <= 3_0 ps;
end vyjyggwct;

entity huom is
  port (qgkiqbh : inout real; x : out real);
end huom;

architecture mgw of huom is
  signal qcywjompo : severity_level;
  signal zkzog : time;
begin
  fqalrm : entity work.uccihv
    port map (zhw => zkzog, mjzqdsxzay => qcywjompo);
  
  -- Single-driven assignments
  x <= 1_0_2.04;
  qgkiqbh <= 16#3_B_9_C_2.C#;
end mgw;

library ieee;
use ieee.std_logic_1164.all;

entity rrvkhsc is
  port (gpwdesjb : buffer real; mg : in std_logic; uxfyhhcfp : buffer std_logic_vector(3 to 4));
end rrvkhsc;

architecture y of rrvkhsc is
  signal di : severity_level;
  signal sdcfirscl : time;
begin
  wclfpeky : entity work.uccihv
    port map (zhw => sdcfirscl, mjzqdsxzay => di);
  
  -- Single-driven assignments
  gpwdesjb <= 2.014;
  
  -- Multi-driven assignments
  uxfyhhcfp <= "LH";
  uxfyhhcfp <= "10";
end y;

library ieee;
use ieee.std_logic_1164.all;

entity vxs is
  port (gsthbogw : inout time; wnsctnutz : out std_logic_vector(1 downto 2));
end vxs;

library ieee;
use ieee.std_logic_1164.all;

architecture fylr of vxs is
  signal bww : std_logic_vector(3 to 4);
  signal sbfvbk : real;
  signal plgd : std_logic_vector(3 to 4);
  signal vkjgvmjzr : std_logic;
  signal yzgvyubkq : real;
  signal xvchruk : severity_level;
  signal sv : severity_level;
  signal tpzv : time;
begin
  zzvl : entity work.uccihv
    port map (zhw => tpzv, mjzqdsxzay => sv);
  orgiezgmim : entity work.uccihv
    port map (zhw => gsthbogw, mjzqdsxzay => xvchruk);
  o : entity work.rrvkhsc
    port map (gpwdesjb => yzgvyubkq, mg => vkjgvmjzr, uxfyhhcfp => plgd);
  ksb : entity work.rrvkhsc
    port map (gpwdesjb => sbfvbk, mg => vkjgvmjzr, uxfyhhcfp => bww);
end fylr;



-- Seed after: 14498953792692190910,5472058987609252853
