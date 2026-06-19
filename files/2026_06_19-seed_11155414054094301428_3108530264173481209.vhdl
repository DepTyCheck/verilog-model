-- Seed: 11155414054094301428,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity czzvremptx is
  port (bwmzh : in integer; xwhoqmeqqn : buffer std_logic; zmcvabprb : inout std_logic_vector(3 to 1); fjkosdivm : in real);
end czzvremptx;

architecture cqfwxklv of czzvremptx is
  
begin
  
end cqfwxklv;

entity mpz is
  port (ii : linkage time; xpy : out real; fgaktondh : out bit_vector(2 downto 2));
end mpz;

library ieee;
use ieee.std_logic_1164.all;

architecture k of mpz is
  signal d : std_logic;
  signal rzrywuef : real;
  signal eir : std_logic_vector(3 to 1);
  signal jyxbyvk : std_logic;
  signal ovi : integer;
begin
  rrxumrvbu : entity work.czzvremptx
    port map (bwmzh => ovi, xwhoqmeqqn => jyxbyvk, zmcvabprb => eir, fjkosdivm => xpy);
  zniksykiuy : entity work.czzvremptx
    port map (bwmzh => ovi, xwhoqmeqqn => jyxbyvk, zmcvabprb => eir, fjkosdivm => rzrywuef);
  jsz : entity work.czzvremptx
    port map (bwmzh => ovi, xwhoqmeqqn => d, zmcvabprb => eir, fjkosdivm => xpy);
  
  -- Single-driven assignments
  fgaktondh <= (others => '0');
  
  -- Multi-driven assignments
  jyxbyvk <= 'U';
  eir <= (others => '0');
  jyxbyvk <= 'Z';
end k;



-- Seed after: 17774713062134303748,3108530264173481209
