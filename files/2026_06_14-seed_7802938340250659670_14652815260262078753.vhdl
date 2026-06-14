-- Seed: 7802938340250659670,14652815260262078753

entity ddfh is
  port (xioli : buffer boolean_vector(2 downto 2));
end ddfh;

architecture oblko of ddfh is
  
begin
  -- Single-driven assignments
  xioli <= (others => TRUE);
end oblko;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (rs : inout time; ioenxnowyf : inout integer; ywshrda : out std_logic_vector(4 to 0));
end j;

architecture cf of j is
  signal qiei : boolean_vector(2 downto 2);
  signal hoicv : boolean_vector(2 downto 2);
  signal prlxgnlmm : boolean_vector(2 downto 2);
  signal jegrpk : boolean_vector(2 downto 2);
begin
  spyjdzry : entity work.ddfh
    port map (xioli => jegrpk);
  hkaerou : entity work.ddfh
    port map (xioli => prlxgnlmm);
  odtyk : entity work.ddfh
    port map (xioli => hoicv);
  mqrce : entity work.ddfh
    port map (xioli => qiei);
  
  -- Single-driven assignments
  ioenxnowyf <= 2#1_1_1#;
  rs <= 2#1.1# ms;
end cf;

entity ygzbi is
  port (wkddzjwzty : buffer real; zl : out boolean_vector(3 downto 2));
end ygzbi;

library ieee;
use ieee.std_logic_1164.all;

architecture i of ygzbi is
  signal pvdc : boolean_vector(2 downto 2);
  signal kco : std_logic_vector(4 to 0);
  signal srfarwmbl : integer;
  signal zogpsqzuj : time;
begin
  eylqqlius : entity work.j
    port map (rs => zogpsqzuj, ioenxnowyf => srfarwmbl, ywshrda => kco);
  vo : entity work.ddfh
    port map (xioli => pvdc);
  
  -- Single-driven assignments
  zl <= (TRUE, FALSE);
  wkddzjwzty <= 8#50447.4_0_3_0#;
  
  -- Multi-driven assignments
  kco <= "";
end i;



-- Seed after: 5594978824527907510,14652815260262078753
