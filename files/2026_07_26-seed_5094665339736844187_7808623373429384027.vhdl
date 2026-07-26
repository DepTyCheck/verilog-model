-- Seed: 5094665339736844187,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity wnsrcdf is
  port (jjhmri : out real; cihwqxj : buffer std_logic; wcxsq : in std_logic_vector(2 to 0); ufqg : out time);
end wnsrcdf;

architecture mwpzbdbr of wnsrcdf is
  
begin
  -- Single-driven assignments
  ufqg <= 1 us;
  jjhmri <= 8#2_1.6_2#;
end mwpzbdbr;

library ieee;
use ieee.std_logic_1164.all;

entity uyxj is
  port (rpkvg : buffer real; xxw : buffer real; wgpkmqcecu : inout std_logic_vector(3 to 1));
end uyxj;

library ieee;
use ieee.std_logic_1164.all;

architecture gequp of uyxj is
  signal m : time;
  signal nsccbacqpm : std_logic_vector(2 to 0);
  signal dx : std_logic;
begin
  ec : entity work.wnsrcdf
    port map (jjhmri => rpkvg, cihwqxj => dx, wcxsq => nsccbacqpm, ufqg => m);
end gequp;



-- Seed after: 1320429181702173935,7808623373429384027
