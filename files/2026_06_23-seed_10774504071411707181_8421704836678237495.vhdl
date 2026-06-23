-- Seed: 10774504071411707181,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity bzvvmjhh is
  port (ivv : inout integer; zspxjyz : buffer std_logic_vector(1 to 2));
end bzvvmjhh;

architecture zwk of bzvvmjhh is
  
begin
  -- Single-driven assignments
  ivv <= 3143;
  
  -- Multi-driven assignments
  zspxjyz <= "11";
  zspxjyz <= "WL";
  zspxjyz <= "0-";
end zwk;

library ieee;
use ieee.std_logic_1164.all;

entity cg is
  port (xcnrks : linkage time; cydpirisw : buffer std_logic);
end cg;

library ieee;
use ieee.std_logic_1164.all;

architecture rchlonyobw of cg is
  signal cfufwkl : std_logic_vector(1 to 2);
  signal dkwbbjp : integer;
  signal hclvws : std_logic_vector(1 to 2);
  signal dzvaeoe : integer;
begin
  bieczkllh : entity work.bzvvmjhh
    port map (ivv => dzvaeoe, zspxjyz => hclvws);
  njxcnyykb : entity work.bzvvmjhh
    port map (ivv => dkwbbjp, zspxjyz => cfufwkl);
  
  -- Multi-driven assignments
  cydpirisw <= 'L';
  cydpirisw <= 'Z';
end rchlonyobw;

entity aehq is
  port (h : out severity_level);
end aehq;

library ieee;
use ieee.std_logic_1164.all;

architecture miulhkxjs of aehq is
  signal ipbhg : std_logic;
  signal hhebz : time;
  signal lsj : integer;
  signal hrhzfedms : std_logic_vector(1 to 2);
  signal gqsl : integer;
  signal mc : std_logic_vector(1 to 2);
  signal icyiqkcq : integer;
begin
  rcfdzupmrm : entity work.bzvvmjhh
    port map (ivv => icyiqkcq, zspxjyz => mc);
  fbvya : entity work.bzvvmjhh
    port map (ivv => gqsl, zspxjyz => hrhzfedms);
  o : entity work.bzvvmjhh
    port map (ivv => lsj, zspxjyz => mc);
  ueyujgq : entity work.cg
    port map (xcnrks => hhebz, cydpirisw => ipbhg);
  
  -- Single-driven assignments
  h <= FAILURE;
end miulhkxjs;



-- Seed after: 13279154318617165796,8421704836678237495
