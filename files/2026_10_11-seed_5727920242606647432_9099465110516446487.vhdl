-- Seed: 5727920242606647432,9099465110516446487

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (nrk : in std_logic_vector(1 downto 1));
end k;

architecture roopto of k is
  
begin
  
end roopto;

library ieee;
use ieee.std_logic_1164.all;

entity nmyvhwchr is
  port (thwhzt : inout std_logic; upxfderbob : inout std_logic; pa : buffer integer_vector(3 downto 0));
end nmyvhwchr;

library ieee;
use ieee.std_logic_1164.all;

architecture mzkjzczyb of nmyvhwchr is
  signal s : std_logic_vector(1 downto 1);
begin
  uljm : entity work.k
    port map (nrk => s);
  knvkpuwe : entity work.k
    port map (nrk => s);
  
  -- Single-driven assignments
  pa <= (2#1_0_1_1_0#, 4144, 8#6_4#, 8#2_6_4#);
  
  -- Multi-driven assignments
  upxfderbob <= upxfderbob;
  upxfderbob <= thwhzt;
end mzkjzczyb;

library ieee;
use ieee.std_logic_1164.all;

entity kprjbogiao is
  port (gd : out boolean; sjumxymzdo : out std_logic);
end kprjbogiao;

architecture qhj of kprjbogiao is
  signal doh : integer_vector(3 downto 0);
  signal lsciux : integer_vector(3 downto 0);
begin
  rygar : entity work.nmyvhwchr
    port map (thwhzt => sjumxymzdo, upxfderbob => sjumxymzdo, pa => lsciux);
  goouzm : entity work.nmyvhwchr
    port map (thwhzt => sjumxymzdo, upxfderbob => sjumxymzdo, pa => doh);
  
  -- Multi-driven assignments
  sjumxymzdo <= 'Z';
  sjumxymzdo <= sjumxymzdo;
  sjumxymzdo <= 'L';
  sjumxymzdo <= sjumxymzdo;
end qhj;

library ieee;
use ieee.std_logic_1164.all;

entity bcs is
  port (oyakty : buffer std_logic);
end bcs;

architecture bfbqhqlsw of bcs is
  
begin
  
end bfbqhqlsw;



-- Seed after: 3709751166981255923,9099465110516446487
