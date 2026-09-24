-- Seed: 14134777892259021092,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity yb is
  port (hdfntvcsly : inout boolean; bud : buffer std_logic_vector(4 downto 2); kl : linkage boolean);
end yb;

architecture y of yb is
  
begin
  -- Single-driven assignments
  hdfntvcsly <= TRUE;
  
  -- Multi-driven assignments
  bud <= ('U', 'L', 'W');
  bud <= bud;
  bud <= bud;
  bud <= bud;
end y;

entity kqdtukwe is
  port (slrxfnss : out integer);
end kqdtukwe;

library ieee;
use ieee.std_logic_1164.all;

architecture ibuc of kqdtukwe is
  signal dcnoktmlud : boolean;
  signal lpov : boolean;
  signal xoteta : boolean;
  signal mrjfv : std_logic_vector(4 downto 2);
  signal nd : boolean;
  signal xvltlgp : boolean;
  signal rxebidrpw : std_logic_vector(4 downto 2);
  signal tdnwqjzowv : boolean;
  signal cchiijgga : boolean;
  signal fcoaa : std_logic_vector(4 downto 2);
  signal odxbmadf : boolean;
begin
  jsxokjy : entity work.yb
    port map (hdfntvcsly => odxbmadf, bud => fcoaa, kl => cchiijgga);
  wtix : entity work.yb
    port map (hdfntvcsly => tdnwqjzowv, bud => rxebidrpw, kl => xvltlgp);
  ur : entity work.yb
    port map (hdfntvcsly => nd, bud => mrjfv, kl => xoteta);
  xkbnpnsgxu : entity work.yb
    port map (hdfntvcsly => lpov, bud => mrjfv, kl => dcnoktmlud);
  
  -- Single-driven assignments
  slrxfnss <= 2#100#;
  
  -- Multi-driven assignments
  rxebidrpw <= fcoaa;
  fcoaa <= fcoaa;
end ibuc;

entity jc is
  port (jx : in integer);
end jc;

library ieee;
use ieee.std_logic_1164.all;

architecture efaqgnhhr of jc is
  signal qhc : boolean;
  signal ikdi : std_logic_vector(4 downto 2);
  signal dlmtymsr : boolean;
  signal vfdtkp : boolean;
  signal wt : std_logic_vector(4 downto 2);
  signal fnbazig : boolean;
  signal fdhqomd : boolean;
  signal fkxybhw : std_logic_vector(4 downto 2);
  signal wkzx : boolean;
  signal yod : integer;
begin
  fmnjckxrwj : entity work.kqdtukwe
    port map (slrxfnss => yod);
  dgtbhrx : entity work.yb
    port map (hdfntvcsly => wkzx, bud => fkxybhw, kl => fdhqomd);
  kdsleg : entity work.yb
    port map (hdfntvcsly => fnbazig, bud => wt, kl => vfdtkp);
  bmkaqv : entity work.yb
    port map (hdfntvcsly => dlmtymsr, bud => ikdi, kl => qhc);
  
  -- Multi-driven assignments
  wt <= "UWL";
  fkxybhw <= fkxybhw;
end efaqgnhhr;

entity sbuldll is
  port (jqommchwkn : in integer; oruinhxl : inout time);
end sbuldll;

architecture bzg of sbuldll is
  signal t : integer;
begin
  bpjktm : entity work.kqdtukwe
    port map (slrxfnss => t);
  
  -- Single-driven assignments
  oruinhxl <= 2#0_0.0# ns;
end bzg;



-- Seed after: 274760266023124240,17234720251424330329
