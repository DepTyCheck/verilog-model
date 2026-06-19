-- Seed: 15009809999323907614,3108530264173481209

entity czef is
  port (oxdruc : buffer integer);
end czef;

architecture duyove of czef is
  
begin
  -- Single-driven assignments
  oxdruc <= 8#1_6_3#;
end duyove;

entity b is
  port (ajd : buffer time);
end b;

architecture w of b is
  signal lqngpszlfx : integer;
  signal zfzw : integer;
  signal qqjksfksth : integer;
  signal qx : integer;
begin
  vsocfsqc : entity work.czef
    port map (oxdruc => qx);
  z : entity work.czef
    port map (oxdruc => qqjksfksth);
  kmro : entity work.czef
    port map (oxdruc => zfzw);
  kgeqsvypk : entity work.czef
    port map (oxdruc => lqngpszlfx);
  
  -- Single-driven assignments
  ajd <= 0 sec;
end w;

library ieee;
use ieee.std_logic_1164.all;

entity lkjm is
  port (od : inout std_logic; facst : linkage real_vector(1 to 2); lubsxxthc : in std_logic_vector(0 downto 1); drnnxgr : buffer time);
end lkjm;

architecture aymuszh of lkjm is
  
begin
  -- Single-driven assignments
  drnnxgr <= 3 sec;
  
  -- Multi-driven assignments
  od <= '0';
end aymuszh;

library ieee;
use ieee.std_logic_1164.all;

entity lhqa is
  port (rcytxnjut : inout time; udgyo : linkage real; iszmag : linkage boolean; ot : in std_logic_vector(1 to 4));
end lhqa;

library ieee;
use ieee.std_logic_1164.all;

architecture rofvhao of lhqa is
  signal xdbk : integer;
  signal xwicm : std_logic_vector(0 downto 1);
  signal qbmsgtczxh : real_vector(1 to 2);
  signal gnec : std_logic;
  signal qslf : time;
begin
  jwlcscx : entity work.b
    port map (ajd => qslf);
  pkjgtbwiam : entity work.lkjm
    port map (od => gnec, facst => qbmsgtczxh, lubsxxthc => xwicm, drnnxgr => rcytxnjut);
  gblt : entity work.czef
    port map (oxdruc => xdbk);
  
  -- Multi-driven assignments
  xwicm <= "";
  xwicm <= "";
  gnec <= 'W';
  gnec <= 'X';
end rofvhao;



-- Seed after: 3307031632980964418,3108530264173481209
