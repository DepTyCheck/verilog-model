-- Seed: 2231769267468110017,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity rko is
  port (vnodcsn : buffer std_logic_vector(0 to 2));
end rko;

architecture zhghnc of rko is
  
begin
  
end zhghnc;

entity lxolirzrev is
  port (wujwrifl : in bit_vector(0 to 4); kczeeas : inout integer; teytd : in real; txkdeys : inout string(4 to 1));
end lxolirzrev;

library ieee;
use ieee.std_logic_1164.all;

architecture sw of lxolirzrev is
  signal ue : std_logic_vector(0 to 2);
  signal wziuemezd : std_logic_vector(0 to 2);
begin
  mczdrafn : entity work.rko
    port map (vnodcsn => wziuemezd);
  qm : entity work.rko
    port map (vnodcsn => ue);
  djynvz : entity work.rko
    port map (vnodcsn => ue);
  jgavm : entity work.rko
    port map (vnodcsn => ue);
  
  -- Single-driven assignments
  txkdeys <= txkdeys;
  kczeeas <= kczeeas;
  
  -- Multi-driven assignments
  ue <= "ZHH";
  ue <= "U1W";
end sw;



-- Seed after: 14913060029263088704,8412319452373742525
