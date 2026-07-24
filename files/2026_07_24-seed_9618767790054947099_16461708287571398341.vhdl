-- Seed: 9618767790054947099,16461708287571398341

entity ahdsp is
  port (whvdogzh : linkage boolean);
end ahdsp;

architecture awgrln of ahdsp is
  
begin
  
end awgrln;

entity drnuxtjgll is
  port (fvsiszhjfb : inout bit; v : inout severity_level; yojivlug : in real; fliz : out bit_vector(1 to 2));
end drnuxtjgll;

architecture jyl of drnuxtjgll is
  signal gwuve : boolean;
  signal myfgicgdgk : boolean;
  signal yqze : boolean;
begin
  qbgtcci : entity work.ahdsp
    port map (whvdogzh => yqze);
  kfiqiolb : entity work.ahdsp
    port map (whvdogzh => myfgicgdgk);
  t : entity work.ahdsp
    port map (whvdogzh => gwuve);
  
  -- Single-driven assignments
  fliz <= fliz;
  v <= v;
  fvsiszhjfb <= fvsiszhjfb;
end jyl;

library ieee;
use ieee.std_logic_1164.all;

entity wcuwsrx is
  port (em : inout std_logic_vector(1 downto 1); fszflcme : in integer; p : in time);
end wcuwsrx;

architecture ycdbeso of wcuwsrx is
  signal rbv : bit_vector(1 to 2);
  signal hikojc : severity_level;
  signal hbohc : bit;
  signal civjviin : bit_vector(1 to 2);
  signal nsht : real;
  signal j : severity_level;
  signal qkjbjlcx : bit;
  signal brs : boolean;
begin
  wieijif : entity work.ahdsp
    port map (whvdogzh => brs);
  mfy : entity work.drnuxtjgll
    port map (fvsiszhjfb => qkjbjlcx, v => j, yojivlug => nsht, fliz => civjviin);
  vathxkr : entity work.drnuxtjgll
    port map (fvsiszhjfb => hbohc, v => hikojc, yojivlug => nsht, fliz => rbv);
  
  -- Single-driven assignments
  nsht <= 2#1_1.0_0_0#;
  
  -- Multi-driven assignments
  em <= em;
  em <= "W";
  em <= em;
  em <= em;
end ycdbeso;



-- Seed after: 18103513284085911228,16461708287571398341
