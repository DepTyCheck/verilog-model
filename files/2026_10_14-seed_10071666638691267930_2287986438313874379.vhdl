-- Seed: 10071666638691267930,2287986438313874379

entity ojxhxuaiae is
  port (cyzjke : buffer integer; htdjzw : out character; tljgdr : buffer time_vector(2 downto 1));
end ojxhxuaiae;

architecture nqlinas of ojxhxuaiae is
  
begin
  
end nqlinas;

entity mc is
  port (esizejdqm : linkage severity_level);
end mc;

architecture pnnzhv of mc is
  signal vyyzgep : time_vector(2 downto 1);
  signal s : character;
  signal dw : integer;
begin
  wl : entity work.ojxhxuaiae
    port map (cyzjke => dw, htdjzw => s, tljgdr => vyyzgep);
end pnnzhv;

entity u is
  port (ojb : inout integer; br : inout time; sdviwfzsga : linkage bit_vector(4 to 1));
end u;

architecture tohhfgv of u is
  signal xmswdi : time_vector(2 downto 1);
  signal obltypwcj : character;
  signal as : integer;
begin
  usxnlaxdr : entity work.ojxhxuaiae
    port map (cyzjke => as, htdjzw => obltypwcj, tljgdr => xmswdi);
  
  -- Single-driven assignments
  br <= 0_4 fs;
  ojb <= as;
end tohhfgv;

library ieee;
use ieee.std_logic_1164.all;

entity wwrdjhqk is
  port (fufvvwou : buffer std_logic; ozty : linkage time; jdcaz : out boolean_vector(4 to 4); m : in time);
end wwrdjhqk;

architecture dunatoeh of wwrdjhqk is
  
begin
  -- Single-driven assignments
  jdcaz <= (others => FALSE);
  
  -- Multi-driven assignments
  fufvvwou <= 'X';
  fufvvwou <= '-';
end dunatoeh;



-- Seed after: 7886876178404031091,2287986438313874379
