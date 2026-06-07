-- Seed: 6512081450012673738,7332793847894666635



entity cbb is
  port (vnlhuibxet : inout bit_vector(1 downto 2); lxymowxh : inout time; hxijwf : inout time);
end cbb;



architecture iemzwxnc of cbb is
  
begin
  
end iemzwxnc;

library ieee;
use ieee.std_logic_1164.all;

entity bkw is
  port (othjdmn : buffer std_logic_vector(0 to 1); br : buffer time; onxcovreg : buffer time);
end bkw;



architecture lyorb of bkw is
  signal nvniqnw : time;
  signal hremvnz : time;
  signal moji : bit_vector(1 downto 2);
  signal xmpfwsqyyw : time;
  signal wkf : bit_vector(1 downto 2);
  signal etspy : time;
  signal kqjbpln : bit_vector(1 downto 2);
begin
  j : entity work.cbb
    port map (vnlhuibxet => kqjbpln, lxymowxh => etspy, hxijwf => onxcovreg);
  gebjilted : entity work.cbb
    port map (vnlhuibxet => wkf, lxymowxh => xmpfwsqyyw, hxijwf => br);
  tyoxdakq : entity work.cbb
    port map (vnlhuibxet => moji, lxymowxh => hremvnz, hxijwf => nvniqnw);
end lyorb;



-- Seed after: 4754456405040726615,7332793847894666635
