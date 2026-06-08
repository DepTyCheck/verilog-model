-- Seed: 6002646924954813870,7142793346053417159

library ieee;
use ieee.std_logic_1164.all;

entity xucykc is
  port (zvh : buffer boolean_vector(2 downto 2); hdodj : inout std_logic_vector(3 to 1));
end xucykc;



architecture key of xucykc is
  
begin
  
end key;



entity fn is
  port (p : in time);
end fn;

library ieee;
use ieee.std_logic_1164.all;

architecture br of fn is
  signal ghsbxc : boolean_vector(2 downto 2);
  signal lvgulqaw : boolean_vector(2 downto 2);
  signal hkekgrfwpo : std_logic_vector(3 to 1);
  signal jpqwnr : boolean_vector(2 downto 2);
begin
  jcbdwzfk : entity work.xucykc
    port map (zvh => jpqwnr, hdodj => hkekgrfwpo);
  qdxbjrus : entity work.xucykc
    port map (zvh => lvgulqaw, hdodj => hkekgrfwpo);
  dddkxcdm : entity work.xucykc
    port map (zvh => ghsbxc, hdodj => hkekgrfwpo);
end br;



-- Seed after: 10830235775537089454,7142793346053417159
