-- Seed: 3673819113858305665,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;

entity bw is
  port (rozjqd : inout std_logic_vector(0 to 3));
end bw;

architecture leawf of bw is
  
begin
  -- Multi-driven assignments
  rozjqd <= ('Z', 'H', '-', 'W');
  rozjqd <= "1XXW";
end leawf;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity pozjyed is
  port (chitwwqjg : in std_logic; yfb : inout integer_value_mirror; jqaehnxy : inout file_subtype_mirror);
end pozjyed;

library ieee;
use ieee.std_logic_1164.all;

architecture ncv of pozjyed is
  signal opicljz : std_logic_vector(0 to 3);
  signal bvdomti : std_logic_vector(0 to 3);
  signal lor : std_logic_vector(0 to 3);
begin
  alezojdq : entity work.bw
    port map (rozjqd => lor);
  ojwyzdz : entity work.bw
    port map (rozjqd => bvdomti);
  ucsusp : entity work.bw
    port map (rozjqd => opicljz);
  ozcvkk : entity work.bw
    port map (rozjqd => opicljz);
  
  -- Multi-driven assignments
  lor <= lor;
  lor <= opicljz;
end ncv;



-- Seed after: 18025671044132052418,3566912872917928779
