-- Seed: 18194031648667690985,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity trea is
  port (wcdptyjpdk : in std_logic_vector(4 to 0); zvipb : inout integer; mdyruipifv : linkage boolean);
end trea;

architecture yvmhv of trea is
  
begin
  -- Single-driven assignments
  zvipb <= 0;
end yvmhv;

library ieee;
use ieee.std_logic_1164.all;

entity suyihwur is
  port (t : inout string(5 downto 2); nh : inout std_logic_vector(1 downto 4); yn : linkage time);
end suyihwur;

architecture kkcqb of suyihwur is
  signal kavwdhwcfj : boolean;
  signal mkirdhp : integer;
  signal wcfbdgapm : boolean;
  signal mqppo : integer;
  signal opqvkgwrq : boolean;
  signal wha : integer;
begin
  tezklekt : entity work.trea
    port map (wcdptyjpdk => nh, zvipb => wha, mdyruipifv => opqvkgwrq);
  dg : entity work.trea
    port map (wcdptyjpdk => nh, zvipb => mqppo, mdyruipifv => wcfbdgapm);
  ym : entity work.trea
    port map (wcdptyjpdk => nh, zvipb => mkirdhp, mdyruipifv => kavwdhwcfj);
  
  -- Single-driven assignments
  t <= "lqui";
  
  -- Multi-driven assignments
  nh <= nh;
  nh <= nh;
end kkcqb;



-- Seed after: 17730877179620132405,10940991575366938685
