-- Seed: 13979734851025910156,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity mmt is
  port (ltnksxxhc : inout bit_vector(0 downto 3); vlh : in integer; lqv : out std_logic_vector(0 to 3));
end mmt;

architecture mj of mmt is
  
begin
  -- Single-driven assignments
  ltnksxxhc <= ltnksxxhc;
  
  -- Multi-driven assignments
  lqv <= lqv;
  lqv <= ('H', '0', 'H', 'H');
  lqv <= ('Z', 'H', 'Z', 'Z');
  lqv <= lqv;
end mj;

entity pdvqgtnxez is
  port (vskpzw : in boolean_vector(0 to 3); kqyy : buffer time; dsbqtbthby : inout time);
end pdvqgtnxez;

library ieee;
use ieee.std_logic_1164.all;

architecture sz of pdvqgtnxez is
  signal zc : std_logic_vector(0 to 3);
  signal edp : bit_vector(0 downto 3);
  signal xxdk : std_logic_vector(0 to 3);
  signal vafodw : integer;
  signal ifvtucy : bit_vector(0 downto 3);
begin
  pcxus : entity work.mmt
    port map (ltnksxxhc => ifvtucy, vlh => vafodw, lqv => xxdk);
  fxcsddvqz : entity work.mmt
    port map (ltnksxxhc => edp, vlh => vafodw, lqv => zc);
  
  -- Multi-driven assignments
  zc <= ('U', 'L', 'L', 'X');
end sz;

library ieee;
use ieee.std_logic_1164.all;

entity rx is
  port (qqtqyqgpp : in std_logic_vector(3 downto 2));
end rx;

library ieee;
use ieee.std_logic_1164.all;

architecture bvqf of rx is
  signal ptkikm : std_logic_vector(0 to 3);
  signal yhperm : bit_vector(0 downto 3);
  signal hbqsmkjwwe : bit_vector(0 downto 3);
  signal it : std_logic_vector(0 to 3);
  signal ur : integer;
  signal ldv : bit_vector(0 downto 3);
begin
  putwys : entity work.mmt
    port map (ltnksxxhc => ldv, vlh => ur, lqv => it);
  upvlcyl : entity work.mmt
    port map (ltnksxxhc => hbqsmkjwwe, vlh => ur, lqv => it);
  yuw : entity work.mmt
    port map (ltnksxxhc => yhperm, vlh => ur, lqv => ptkikm);
  
  -- Single-driven assignments
  ur <= ur;
  
  -- Multi-driven assignments
  ptkikm <= ('1', 'U', 'X', 'U');
  ptkikm <= it;
  it <= ('-', 'Z', '-', 'Z');
  it <= ptkikm;
end bvqf;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (qaj : linkage integer; edvzwvjp : linkage std_logic; djdmblwpa : out integer);
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture ehfmnkwcns of m is
  signal lgapq : std_logic_vector(3 downto 2);
  signal wmkeglwl : std_logic_vector(0 to 3);
  signal jnxrllsiq : integer;
  signal fzw : bit_vector(0 downto 3);
  signal yd : time;
  signal lqk : time;
  signal v : boolean_vector(0 to 3);
begin
  izqxbmmlsb : entity work.pdvqgtnxez
    port map (vskpzw => v, kqyy => lqk, dsbqtbthby => yd);
  pe : entity work.mmt
    port map (ltnksxxhc => fzw, vlh => jnxrllsiq, lqv => wmkeglwl);
  hizhqx : entity work.rx
    port map (qqtqyqgpp => lgapq);
  
  -- Single-driven assignments
  djdmblwpa <= djdmblwpa;
  jnxrllsiq <= djdmblwpa;
  v <= v;
  
  -- Multi-driven assignments
  wmkeglwl <= wmkeglwl;
  wmkeglwl <= ('L', '-', 'W', 'L');
end ehfmnkwcns;



-- Seed after: 3559507290119645189,13788294582928374589
