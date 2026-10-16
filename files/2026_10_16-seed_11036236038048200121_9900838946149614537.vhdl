-- Seed: 11036236038048200121,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity pswzqj is
  port (odruq : in std_logic);
end pswzqj;

architecture kivglolzxe of pswzqj is
  
begin
  
end kivglolzxe;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (bhytg : buffer std_logic_vector(0 downto 0); yxbt : inout time; wkvjrm : in std_logic_vector(1 to 0); ceeuz : inout time);
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture wcbsyy of f is
  signal saa : std_logic;
  signal sywwvs : std_logic;
  signal zcuedzbxe : std_logic;
  signal ens : std_logic;
begin
  idp : entity work.pswzqj
    port map (odruq => ens);
  hk : entity work.pswzqj
    port map (odruq => zcuedzbxe);
  pvuhkmi : entity work.pswzqj
    port map (odruq => sywwvs);
  wiaenhvpl : entity work.pswzqj
    port map (odruq => saa);
  
  -- Multi-driven assignments
  ens <= 'X';
  bhytg <= bhytg;
  bhytg <= "1";
  bhytg <= (others => '-');
end wcbsyy;

entity jo is
  port (uer : inout severity_level);
end jo;

library ieee;
use ieee.std_logic_1164.all;

architecture uelkjpifan of jo is
  signal fuhu : std_logic;
begin
  icsqss : entity work.pswzqj
    port map (odruq => fuhu);
  hgbbynaej : entity work.pswzqj
    port map (odruq => fuhu);
  
  -- Multi-driven assignments
  fuhu <= 'W';
end uelkjpifan;

entity yilw is
  port (crk : out real; h : inout real);
end yilw;

architecture vwxpuidv of yilw is
  
begin
  -- Single-driven assignments
  h <= h;
  crk <= 2#0_1_0.1_0_0_0_1#;
end vwxpuidv;



-- Seed after: 14505890398521686644,9900838946149614537
