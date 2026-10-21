-- Seed: 13716902812536992951,17219977430298397867

entity vggeex is
  port (rhhr : linkage string(4 downto 4); sudzfkzwt : in time; nhqfoba : linkage bit; kgddbtuu : linkage bit_vector(3 downto 0));
end vggeex;

architecture bufatc of vggeex is
  
begin
  
end bufatc;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (hw : out std_logic_vector(4 downto 0); xqpkmma : in bit);
end s;

architecture cvv of s is
  signal d : bit_vector(3 downto 0);
  signal e : bit;
  signal ge : string(4 downto 4);
  signal lfnpzhnxj : bit_vector(3 downto 0);
  signal tnnfptbe : bit;
  signal uezlfvrxp : string(4 downto 4);
  signal biahxk : bit_vector(3 downto 0);
  signal mqxrdbfd : bit;
  signal mswrwsorcx : time;
  signal iz : string(4 downto 4);
begin
  imja : entity work.vggeex
    port map (rhhr => iz, sudzfkzwt => mswrwsorcx, nhqfoba => mqxrdbfd, kgddbtuu => biahxk);
  pjknolpc : entity work.vggeex
    port map (rhhr => uezlfvrxp, sudzfkzwt => mswrwsorcx, nhqfoba => tnnfptbe, kgddbtuu => lfnpzhnxj);
  vimfz : entity work.vggeex
    port map (rhhr => ge, sudzfkzwt => mswrwsorcx, nhqfoba => e, kgddbtuu => d);
  
  -- Single-driven assignments
  mswrwsorcx <= mswrwsorcx;
end cvv;

entity ydqvgaj is
  port (qvzw : linkage real);
end ydqvgaj;

library ieee;
use ieee.std_logic_1164.all;

architecture cfbwmurx of ydqvgaj is
  signal v : bit;
  signal reporybkl : std_logic_vector(4 downto 0);
  signal omomosdqye : bit_vector(3 downto 0);
  signal sszeco : bit;
  signal a : time;
  signal r : string(4 downto 4);
  signal o : bit;
  signal ufxd : std_logic_vector(4 downto 0);
begin
  vrllouok : entity work.s
    port map (hw => ufxd, xqpkmma => o);
  wlknql : entity work.vggeex
    port map (rhhr => r, sudzfkzwt => a, nhqfoba => sszeco, kgddbtuu => omomosdqye);
  epgvfa : entity work.s
    port map (hw => reporybkl, xqpkmma => v);
  
  -- Single-driven assignments
  a <= a;
  
  -- Multi-driven assignments
  reporybkl <= ufxd;
end cfbwmurx;

library ieee;
use ieee.std_logic_1164.all;

entity dqndchd is
  port (ory : linkage std_logic_vector(3 to 3); bxnhnevr : out real; jvqdbafy : in time);
end dqndchd;

architecture vavudikc of dqndchd is
  signal jvkql : real;
begin
  hxqyebduo : entity work.ydqvgaj
    port map (qvzw => bxnhnevr);
  uak : entity work.ydqvgaj
    port map (qvzw => jvkql);
end vavudikc;



-- Seed after: 10963545435823549303,17219977430298397867
