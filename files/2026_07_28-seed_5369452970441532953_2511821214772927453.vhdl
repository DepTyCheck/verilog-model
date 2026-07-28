-- Seed: 5369452970441532953,2511821214772927453

entity l is
  port (udki : inout real);
end l;

architecture tpib of l is
  
begin
  -- Single-driven assignments
  udki <= 3402.2;
end tpib;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (ezolifqtc : buffer real; rynzpzvzub : out time; zglkgagapg : buffer std_logic_vector(2 to 0); bppxoznb : linkage integer_vector(3 downto 3));
end r;

architecture reickpmnq of r is
  signal xroxbfo : real;
begin
  oo : entity work.l
    port map (udki => xroxbfo);
  cdfivheg : entity work.l
    port map (udki => ezolifqtc);
  
  -- Single-driven assignments
  rynzpzvzub <= 1 ns;
end reickpmnq;



-- Seed after: 773235865077282019,2511821214772927453
