-- Seed: 13132622475006260699,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity nsjruq is
  port (kegtbcdw : inout bit; gkle : inout std_logic; iepw : buffer boolean_vector(0 to 2); yng : linkage std_logic_vector(1 to 0));
end nsjruq;

architecture wogq of nsjruq is
  
begin
  -- Single-driven assignments
  iepw <= (TRUE, FALSE, FALSE);
  kegtbcdw <= '1';
  
  -- Multi-driven assignments
  gkle <= '1';
  gkle <= '-';
end wogq;

entity sqxfqrb is
  port (qfscqmpt : buffer real; msunicw : linkage real);
end sqxfqrb;

library ieee;
use ieee.std_logic_1164.all;

architecture qw of sqxfqrb is
  signal ermhv : std_logic_vector(1 to 0);
  signal jckc : boolean_vector(0 to 2);
  signal vahum : std_logic;
  signal xusth : bit;
begin
  izn : entity work.nsjruq
    port map (kegtbcdw => xusth, gkle => vahum, iepw => jckc, yng => ermhv);
  
  -- Multi-driven assignments
  vahum <= 'L';
  vahum <= 'Z';
  vahum <= '-';
end qw;

library ieee;
use ieee.std_logic_1164.all;

entity uoye is
  port (ngkgxs : inout bit_vector(4 to 1); yttnf : in std_logic_vector(4 downto 2); ymesofnsc : in integer; ob : inout real);
end uoye;

library ieee;
use ieee.std_logic_1164.all;

architecture vjswu of uoye is
  signal wpvljkuk : boolean_vector(0 to 2);
  signal tpbkageq : bit;
  signal jxvckx : boolean_vector(0 to 2);
  signal s : bit;
  signal xx : std_logic_vector(1 to 0);
  signal ht : boolean_vector(0 to 2);
  signal qyqie : bit;
  signal lzqgkzpebh : std_logic_vector(1 to 0);
  signal vdfq : boolean_vector(0 to 2);
  signal mkjybn : std_logic;
  signal lcrnzm : bit;
begin
  bge : entity work.nsjruq
    port map (kegtbcdw => lcrnzm, gkle => mkjybn, iepw => vdfq, yng => lzqgkzpebh);
  lxqso : entity work.nsjruq
    port map (kegtbcdw => qyqie, gkle => mkjybn, iepw => ht, yng => xx);
  mgayhtbmbm : entity work.nsjruq
    port map (kegtbcdw => s, gkle => mkjybn, iepw => jxvckx, yng => lzqgkzpebh);
  qbphga : entity work.nsjruq
    port map (kegtbcdw => tpbkageq, gkle => mkjybn, iepw => wpvljkuk, yng => xx);
  
  -- Single-driven assignments
  ngkgxs <= (others => '0');
  
  -- Multi-driven assignments
  lzqgkzpebh <= (others => '0');
  xx <= "";
end vjswu;



-- Seed after: 11799257979050131863,3924983747739634027
