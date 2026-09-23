-- Seed: 1332826427384827568,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (kyudagctof : in std_logic_vector(0 downto 1); mv : linkage time_vector(1 downto 3));
end u;

architecture zyadkuuvj of u is
  
begin
  
end zyadkuuvj;

entity p is
  port (wzpg : buffer boolean_vector(3 downto 0); rc : linkage integer_vector(3 to 2); zkjqi : out integer; ay : linkage real);
end p;

architecture cpauxnekr of p is
  
begin
  -- Single-driven assignments
  zkjqi <= 8#20266#;
  wzpg <= (TRUE, FALSE, TRUE, FALSE);
end cpauxnekr;

entity aru is
  port (lhfchfhju : out boolean_vector(3 downto 2); nxwm : out real; tpix : linkage time_vector(3 downto 1); bfupvta : buffer time_vector(4 to 2));
end aru;

library ieee;
use ieee.std_logic_1164.all;

architecture v of aru is
  signal ancjezqoam : time_vector(1 downto 3);
  signal uqe : time_vector(1 downto 3);
  signal pkosdp : std_logic_vector(0 downto 1);
begin
  fmlprobdah : entity work.u
    port map (kyudagctof => pkosdp, mv => uqe);
  thdfkoqofm : entity work.u
    port map (kyudagctof => pkosdp, mv => ancjezqoam);
  wkmeclxaz : entity work.u
    port map (kyudagctof => pkosdp, mv => bfupvta);
  
  -- Multi-driven assignments
  pkosdp <= pkosdp;
  pkosdp <= "";
end v;



-- Seed after: 5569806232109223123,8067602802092121131
