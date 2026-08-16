-- Seed: 3518182775315825215,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity osko is
  port (vbuzhbl : in std_logic_vector(4 downto 1); nghwytxzxq : buffer string(3 downto 4));
end osko;

architecture svce of osko is
  
begin
  
end svce;

entity vilhpsrsup is
  port (buvjiekqp : buffer integer; bc : in real);
end vilhpsrsup;

library ieee;
use ieee.std_logic_1164.all;

architecture umdcijdip of vilhpsrsup is
  signal qd : string(3 downto 4);
  signal ggd : std_logic_vector(4 downto 1);
  signal duphesnhf : string(3 downto 4);
  signal pltghq : string(3 downto 4);
  signal oqaeig : string(3 downto 4);
  signal ehzvg : std_logic_vector(4 downto 1);
begin
  ehyefhyq : entity work.osko
    port map (vbuzhbl => ehzvg, nghwytxzxq => oqaeig);
  zeksaon : entity work.osko
    port map (vbuzhbl => ehzvg, nghwytxzxq => pltghq);
  wgsktlusuh : entity work.osko
    port map (vbuzhbl => ehzvg, nghwytxzxq => duphesnhf);
  oznffbf : entity work.osko
    port map (vbuzhbl => ggd, nghwytxzxq => qd);
  
  -- Single-driven assignments
  buvjiekqp <= buvjiekqp;
  
  -- Multi-driven assignments
  ehzvg <= "HWHU";
  ehzvg <= "LH1X";
  ehzvg <= ehzvg;
  ehzvg <= ggd;
end umdcijdip;

library ieee;
use ieee.std_logic_1164.all;

entity flznud is
  port (hywuwhmrm : buffer std_logic; jqjbcrreil : in real_vector(0 downto 2));
end flznud;

library ieee;
use ieee.std_logic_1164.all;

architecture copz of flznud is
  signal pafkwl : string(3 downto 4);
  signal lwwvubfpa : std_logic_vector(4 downto 1);
begin
  beo : entity work.osko
    port map (vbuzhbl => lwwvubfpa, nghwytxzxq => pafkwl);
end copz;

library ieee;
use ieee.std_logic_1164.all;

entity ppwb is
  port (cvoybpouwo : inout std_logic; k : inout bit; dwkcoz : in integer_vector(0 to 0));
end ppwb;

library ieee;
use ieee.std_logic_1164.all;

architecture g of ppwb is
  signal lrpvozigv : string(3 downto 4);
  signal roxa : std_logic_vector(4 downto 1);
  signal j : real_vector(0 downto 2);
  signal iwua : std_logic;
begin
  zynix : entity work.flznud
    port map (hywuwhmrm => iwua, jqjbcrreil => j);
  kekor : entity work.osko
    port map (vbuzhbl => roxa, nghwytxzxq => lrpvozigv);
  
  -- Single-driven assignments
  k <= k;
  j <= j;
end g;



-- Seed after: 10554933956147261863,13857275728440271305
