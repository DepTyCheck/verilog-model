-- Seed: 12369634572703637790,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity ocgijxpptf is
  port (el : linkage std_logic_vector(3 to 2); x : inout std_logic_vector(0 to 4));
end ocgijxpptf;

architecture piatzbqj of ocgijxpptf is
  
begin
  -- Multi-driven assignments
  x <= "H-HZX";
  x <= "0ZU-1";
  x <= x;
end piatzbqj;

entity jmgwfqwzp is
  port (yu : buffer real; hbwpybbvhj : buffer integer; lqnbdobcwo : linkage boolean);
end jmgwfqwzp;

library ieee;
use ieee.std_logic_1164.all;

architecture qekch of jmgwfqwzp is
  signal nvth : std_logic_vector(3 to 2);
  signal dgcytahju : std_logic_vector(0 to 4);
  signal sqejyru : std_logic_vector(3 to 2);
  signal eho : std_logic_vector(0 to 4);
  signal lifionuka : std_logic_vector(3 to 2);
begin
  mydugstn : entity work.ocgijxpptf
    port map (el => lifionuka, x => eho);
  vozro : entity work.ocgijxpptf
    port map (el => sqejyru, x => dgcytahju);
  qanavdo : entity work.ocgijxpptf
    port map (el => lifionuka, x => dgcytahju);
  ofvb : entity work.ocgijxpptf
    port map (el => nvth, x => eho);
  
  -- Single-driven assignments
  hbwpybbvhj <= 2400;
  yu <= 2#1_0_0_1.01001#;
  
  -- Multi-driven assignments
  lifionuka <= "";
  lifionuka <= "";
end qekch;



-- Seed after: 9452653608744862491,15795020531041709203
