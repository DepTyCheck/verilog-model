-- Seed: 2593127971572776035,16159265764638711791

entity rlq is
  port (flfzxq : inout integer; phbuj : out real_vector(4 downto 1); svxg : in character; uzrtlawcau : in severity_level);
end rlq;

architecture tdp of rlq is
  
begin
  
end tdp;

entity sesybktscn is
  port (awlwvmtl : in time; wy : linkage boolean);
end sesybktscn;

architecture chabpjdjb of sesybktscn is
  signal dmgf : real_vector(4 downto 1);
  signal moye : integer;
  signal yqp : severity_level;
  signal uttgcrrl : character;
  signal mczrl : real_vector(4 downto 1);
  signal slegbyglxw : integer;
begin
  gcofhfgiot : entity work.rlq
    port map (flfzxq => slegbyglxw, phbuj => mczrl, svxg => uttgcrrl, uzrtlawcau => yqp);
  woytqg : entity work.rlq
    port map (flfzxq => moye, phbuj => dmgf, svxg => uttgcrrl, uzrtlawcau => yqp);
  
  -- Single-driven assignments
  uttgcrrl <= uttgcrrl;
end chabpjdjb;

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (oz : out bit; qg : out std_logic; rnvecutrch : linkage integer; q : out integer);
end z;

architecture sigr of z is
  signal itxcrnewy : boolean;
  signal hzpvqgoro : boolean;
  signal flv : time;
  signal qqf : boolean;
  signal c : time;
  signal wzusk : severity_level;
  signal lm : character;
  signal tncfpcokbo : real_vector(4 downto 1);
begin
  ojgxjwqtm : entity work.rlq
    port map (flfzxq => q, phbuj => tncfpcokbo, svxg => lm, uzrtlawcau => wzusk);
  irsqwte : entity work.sesybktscn
    port map (awlwvmtl => c, wy => qqf);
  nwexst : entity work.sesybktscn
    port map (awlwvmtl => flv, wy => hzpvqgoro);
  mv : entity work.sesybktscn
    port map (awlwvmtl => c, wy => itxcrnewy);
  
  -- Single-driven assignments
  flv <= c;
  oz <= '1';
  c <= 1 min;
  
  -- Multi-driven assignments
  qg <= '1';
end sigr;

entity o is
  port (zpenhz : inout real; liwjffpj : linkage integer_vector(1 to 1); rhxg : linkage real; nizpsxpsue : inout real);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture d of o is
  signal hvqhvteph : severity_level;
  signal egjwy : character;
  signal ja : real_vector(4 downto 1);
  signal fjbghrbk : integer;
  signal mmox : integer;
  signal dzco : integer;
  signal s : std_logic;
  signal r : bit;
begin
  yopqa : entity work.z
    port map (oz => r, qg => s, rnvecutrch => dzco, q => mmox);
  md : entity work.rlq
    port map (flfzxq => fjbghrbk, phbuj => ja, svxg => egjwy, uzrtlawcau => hvqhvteph);
  
  -- Multi-driven assignments
  s <= s;
end d;



-- Seed after: 18129916160665306804,16159265764638711791
