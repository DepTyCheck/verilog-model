-- Seed: 3489825406103361081,13479070923501788437

entity rzorwrph is
  port (gmed : buffer real; xjpmcjhtbr : linkage real);
end rzorwrph;

architecture kmzgiicpj of rzorwrph is
  
begin
  
end kmzgiicpj;

library ieee;
use ieee.std_logic_1164.all;

entity fje is
  port (bhxwcdhheg : out std_logic_vector(4 downto 3));
end fje;

architecture b of fje is
  signal s : real;
  signal oxqyvb : real;
begin
  yxxhqjwzfu : entity work.rzorwrph
    port map (gmed => oxqyvb, xjpmcjhtbr => s);
end b;

entity fc is
  port (aslz : linkage integer; wszcmr : out real; tgyuhuxifu : inout integer);
end fc;

library ieee;
use ieee.std_logic_1164.all;

architecture gpkewrom of fc is
  signal qrusxyxh : real;
  signal vzigq : real;
  signal z : std_logic_vector(4 downto 3);
begin
  czae : entity work.fje
    port map (bhxwcdhheg => z);
  wmorkb : entity work.rzorwrph
    port map (gmed => vzigq, xjpmcjhtbr => qrusxyxh);
  sxbgvcwp : entity work.fje
    port map (bhxwcdhheg => z);
  
  -- Single-driven assignments
  tgyuhuxifu <= 44132;
  wszcmr <= 1201.3;
  
  -- Multi-driven assignments
  z <= ('U', 'L');
  z <= "0Z";
  z <= "U0";
  z <= ('X', 'W');
end gpkewrom;

entity nun is
  port (jqitsws : linkage bit_vector(1 downto 2));
end nun;

architecture b of nun is
  signal bjvqkr : real;
  signal swd : real;
begin
  rgmsjeuzrw : entity work.rzorwrph
    port map (gmed => swd, xjpmcjhtbr => bjvqkr);
end b;



-- Seed after: 11514352907075624772,13479070923501788437
