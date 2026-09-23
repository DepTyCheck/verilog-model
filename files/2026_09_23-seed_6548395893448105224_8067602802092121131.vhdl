-- Seed: 6548395893448105224,8067602802092121131

entity iqvwmwag is
  port (fatoxvnl : out real; lp : in bit; ltsfhikuu : inout boolean_vector(0 to 3));
end iqvwmwag;

architecture lmdioghci of iqvwmwag is
  
begin
  -- Single-driven assignments
  ltsfhikuu <= (TRUE, TRUE, TRUE, FALSE);
end lmdioghci;

library ieee;
use ieee.std_logic_1164.all;

entity qxvpxcwcka is
  port (b : inout real_vector(2 to 4); xwvbdt : inout time_vector(1 downto 3); fzu : linkage std_logic; zgrfsxcao : out real);
end qxvpxcwcka;

architecture bqqvmydym of qxvpxcwcka is
  signal jkl : boolean_vector(0 to 3);
  signal lm : real;
  signal a : boolean_vector(0 to 3);
  signal qubn : bit;
  signal k : boolean_vector(0 to 3);
  signal auvou : real;
  signal sgvqoefdmk : boolean_vector(0 to 3);
  signal qvnmq : bit;
  signal dzafnlkujw : real;
begin
  dbmx : entity work.iqvwmwag
    port map (fatoxvnl => dzafnlkujw, lp => qvnmq, ltsfhikuu => sgvqoefdmk);
  joksltgbgg : entity work.iqvwmwag
    port map (fatoxvnl => auvou, lp => qvnmq, ltsfhikuu => k);
  nukab : entity work.iqvwmwag
    port map (fatoxvnl => zgrfsxcao, lp => qubn, ltsfhikuu => a);
  utjr : entity work.iqvwmwag
    port map (fatoxvnl => lm, lp => qvnmq, ltsfhikuu => jkl);
end bqqvmydym;

entity gc is
  port (zaigfv : linkage real_vector(0 downto 0); yhxuh : in time; qouycssmwk : out integer_vector(2 to 3); lrmq : in boolean);
end gc;

library ieee;
use ieee.std_logic_1164.all;

architecture eoibuzkx of gc is
  signal m : real;
  signal okqn : std_logic;
  signal bvtafm : time_vector(1 downto 3);
  signal atarmmdy : real_vector(2 to 4);
begin
  q : entity work.qxvpxcwcka
    port map (b => atarmmdy, xwvbdt => bvtafm, fzu => okqn, zgrfsxcao => m);
  
  -- Single-driven assignments
  qouycssmwk <= (8#2_7_4_1_0#, 2#0110#);
  
  -- Multi-driven assignments
  okqn <= okqn;
  okqn <= 'L';
  okqn <= okqn;
end eoibuzkx;



-- Seed after: 6484075537417056482,8067602802092121131
