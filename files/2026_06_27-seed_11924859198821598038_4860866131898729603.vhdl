-- Seed: 11924859198821598038,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity bwwrz is
  port ( drdgsmu : buffer std_logic
  ; jxl : out time_vector(1 to 2)
  ; pgdnwwy : buffer std_logic_vector(1 downto 1)
  ; thgfifm : inout std_logic_vector(1 to 0)
  );
end bwwrz;

architecture wf of bwwrz is
  
begin
  -- Single-driven assignments
  jxl <= (8#4_4_5_0_4.2_5_0# ps, 1 hr);
  
  -- Multi-driven assignments
  thgfifm <= "";
  pgdnwwy <= "-";
  thgfifm <= (others => '0');
end wf;

entity wyeyvigf is
  port (tqecv : out real);
end wyeyvigf;

library ieee;
use ieee.std_logic_1164.all;

architecture f of wyeyvigf is
  signal zi : std_logic_vector(1 to 0);
  signal uasfeenyiz : time_vector(1 to 2);
  signal blvetf : std_logic_vector(1 to 0);
  signal rsalxti : std_logic_vector(1 downto 1);
  signal hx : time_vector(1 to 2);
  signal tcixdgd : std_logic;
begin
  qvyh : entity work.bwwrz
    port map (drdgsmu => tcixdgd, jxl => hx, pgdnwwy => rsalxti, thgfifm => blvetf);
  hv : entity work.bwwrz
    port map (drdgsmu => tcixdgd, jxl => uasfeenyiz, pgdnwwy => rsalxti, thgfifm => zi);
  
  -- Single-driven assignments
  tqecv <= 3.4_2;
end f;

library ieee;
use ieee.std_logic_1164.all;

entity rnc is
  port (ed : inout std_logic_vector(4 to 2); j : linkage std_logic_vector(4 to 2); gyk : buffer integer);
end rnc;

architecture regsrvf of rnc is
  signal xi : real;
  signal tln : real;
begin
  grok : entity work.wyeyvigf
    port map (tqecv => tln);
  vhx : entity work.wyeyvigf
    port map (tqecv => xi);
  
  -- Single-driven assignments
  gyk <= 16#9#;
  
  -- Multi-driven assignments
  ed <= "";
  ed <= "";
end regsrvf;



-- Seed after: 1983676388745899668,4860866131898729603
