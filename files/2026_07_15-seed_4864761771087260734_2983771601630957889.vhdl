-- Seed: 4864761771087260734,2983771601630957889

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity w is
  port ( variable fjkrrtiyo : inout record_value_mirror_pt
  ; sdvvh : inout real_vector(2 downto 0)
  ; bzzdlipis : buffer std_logic_vector(1 downto 4)
  ; p : out integer
  );
end w;

architecture r of w is
  
begin
  -- Multi-driven assignments
  bzzdlipis <= bzzdlipis;
  bzzdlipis <= "";
  bzzdlipis <= (others => '0');
end r;

entity fpb is
  port (igjpy : in integer);
end fpb;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture laayqpbbo of fpb is
  signal mznvn : integer;
  signal wnwdlkux : real_vector(2 downto 0);
  shared variable yqewdrilu : record_value_mirror_pt;
  signal et : integer;
  signal keokefjjl : std_logic_vector(1 downto 4);
  signal pwowmkzvj : real_vector(2 downto 0);
  shared variable r : record_value_mirror_pt;
  signal eentojt : integer;
  signal wdgzhych : real_vector(2 downto 0);
  shared variable gpiropca : record_value_mirror_pt;
  signal ytiolclay : integer;
  signal qvyb : std_logic_vector(1 downto 4);
  signal qzcounf : real_vector(2 downto 0);
  shared variable imqnv : record_value_mirror_pt;
begin
  idofaqvxxi : entity work.w
    port map (fjkrrtiyo => imqnv, sdvvh => qzcounf, bzzdlipis => qvyb, p => ytiolclay);
  khfzwcj : entity work.w
    port map (fjkrrtiyo => gpiropca, sdvvh => wdgzhych, bzzdlipis => qvyb, p => eentojt);
  f : entity work.w
    port map (fjkrrtiyo => r, sdvvh => pwowmkzvj, bzzdlipis => keokefjjl, p => et);
  cairplmcw : entity work.w
    port map (fjkrrtiyo => yqewdrilu, sdvvh => wnwdlkux, bzzdlipis => keokefjjl, p => mznvn);
end laayqpbbo;



-- Seed after: 7658678291283738478,2983771601630957889
