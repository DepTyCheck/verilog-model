-- Seed: 2756461323733881343,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity zwhnt is
  port ( ldrueh : in std_logic_vector(3 to 0)
  ; d : linkage bit_vector(2 to 4)
  ; hv : out std_logic_vector(0 to 0)
  ; kybkwy : linkage std_logic_vector(0 to 1)
  );
end zwhnt;

architecture dccvrpvb of zwhnt is
  
begin
  -- Multi-driven assignments
  hv <= hv;
end dccvrpvb;

entity yvigmqmtc is
  port (ranxjafp : in bit_vector(4 to 1); ppchokhos : inout integer);
end yvigmqmtc;

library ieee;
use ieee.std_logic_1164.all;

architecture njdesj of yvigmqmtc is
  signal viqmufusm : std_logic_vector(0 to 1);
  signal hmmhy : bit_vector(2 to 4);
  signal ztpsvmhdgm : bit_vector(2 to 4);
  signal ugr : std_logic_vector(0 to 1);
  signal wz : std_logic_vector(0 to 0);
  signal iytwwgz : bit_vector(2 to 4);
  signal jflhmxyu : std_logic_vector(3 to 0);
begin
  ijyzkug : entity work.zwhnt
    port map (ldrueh => jflhmxyu, d => iytwwgz, hv => wz, kybkwy => ugr);
  z : entity work.zwhnt
    port map (ldrueh => jflhmxyu, d => ztpsvmhdgm, hv => wz, kybkwy => ugr);
  e : entity work.zwhnt
    port map (ldrueh => jflhmxyu, d => hmmhy, hv => wz, kybkwy => viqmufusm);
  
  -- Multi-driven assignments
  jflhmxyu <= "";
  jflhmxyu <= (others => '0');
  ugr <= ('Z', 'Z');
end njdesj;



-- Seed after: 5178864438619074761,16461708287571398341
