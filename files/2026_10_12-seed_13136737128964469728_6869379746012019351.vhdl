-- Seed: 13136737128964469728,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity iwhx is
  port (wi : inout std_logic; ldzzycpyx : out boolean; v : linkage integer);
end iwhx;

architecture vzzf of iwhx is
  
begin
  -- Single-driven assignments
  ldzzycpyx <= FALSE;
  
  -- Multi-driven assignments
  wi <= '0';
  wi <= wi;
end vzzf;

entity loeafckzx is
  port (mhb : out real);
end loeafckzx;

library ieee;
use ieee.std_logic_1164.all;

architecture shox of loeafckzx is
  signal mtxfli : integer;
  signal njfjwkyo : boolean;
  signal wmrtqywt : integer;
  signal iy : boolean;
  signal tpignutm : std_logic;
  signal mtvbdj : integer;
  signal mq : boolean;
  signal gwdcccyduk : integer;
  signal s : boolean;
  signal jr : std_logic;
begin
  fx : entity work.iwhx
    port map (wi => jr, ldzzycpyx => s, v => gwdcccyduk);
  idpc : entity work.iwhx
    port map (wi => jr, ldzzycpyx => mq, v => mtvbdj);
  ijrxlvaqxg : entity work.iwhx
    port map (wi => tpignutm, ldzzycpyx => iy, v => wmrtqywt);
  bpblpr : entity work.iwhx
    port map (wi => tpignutm, ldzzycpyx => njfjwkyo, v => mtxfli);
  
  -- Single-driven assignments
  mhb <= 444.4003;
end shox;

entity nprhlhwedl is
  port (moro : inout boolean_vector(2 downto 3));
end nprhlhwedl;

architecture hgeiuyrnvy of nprhlhwedl is
  
begin
  -- Single-driven assignments
  moro <= (others => TRUE);
end hgeiuyrnvy;



-- Seed after: 1412562856966114616,6869379746012019351
