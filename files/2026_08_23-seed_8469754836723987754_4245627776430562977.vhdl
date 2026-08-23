-- Seed: 8469754836723987754,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity aqt is
  port (pvswyby : in bit_vector(4 to 0); nweoetxm : inout character; fovwabmkey : in std_logic_vector(3 to 1));
end aqt;

architecture owzwwvqn of aqt is
  
begin
  -- Single-driven assignments
  nweoetxm <= 'v';
end owzwwvqn;

entity ghluouy is
  port (tlz : inout integer; igmnjabnc : in real; rglsmmv : inout boolean);
end ghluouy;

library ieee;
use ieee.std_logic_1164.all;

architecture cyw of ghluouy is
  signal cludmc : std_logic_vector(3 to 1);
  signal ugbogci : character;
  signal hbftgo : std_logic_vector(3 to 1);
  signal w : character;
  signal uzblnpf : character;
  signal dhy : bit_vector(4 to 0);
  signal y : std_logic_vector(3 to 1);
  signal nubrtz : character;
  signal anjcfwhf : bit_vector(4 to 0);
begin
  wlswno : entity work.aqt
    port map (pvswyby => anjcfwhf, nweoetxm => nubrtz, fovwabmkey => y);
  kwkwyyfofz : entity work.aqt
    port map (pvswyby => dhy, nweoetxm => uzblnpf, fovwabmkey => y);
  avwhe : entity work.aqt
    port map (pvswyby => anjcfwhf, nweoetxm => w, fovwabmkey => hbftgo);
  yykg : entity work.aqt
    port map (pvswyby => anjcfwhf, nweoetxm => ugbogci, fovwabmkey => cludmc);
  
  -- Single-driven assignments
  anjcfwhf <= (others => '0');
  dhy <= anjcfwhf;
  
  -- Multi-driven assignments
  y <= (others => '0');
  y <= "";
  y <= "";
end cyw;



-- Seed after: 1778362776259097378,4245627776430562977
