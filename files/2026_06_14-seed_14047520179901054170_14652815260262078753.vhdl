-- Seed: 14047520179901054170,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity fmqd is
  port (guhn : in std_logic; qicr : in real; zlapthj : buffer std_logic; zjzbiyk : buffer std_logic_vector(2 to 4));
end fmqd;

architecture dxwkd of fmqd is
  
begin
  -- Multi-driven assignments
  zlapthj <= 'U';
end dxwkd;

entity xfs is
  port (gjuq : in time; msjacahe : inout character);
end xfs;

library ieee;
use ieee.std_logic_1164.all;

architecture qvz of xfs is
  signal wh : std_logic_vector(2 to 4);
  signal esf : std_logic_vector(2 to 4);
  signal ic : std_logic;
  signal utphupo : real;
  signal tbnqhxzjcv : std_logic_vector(2 to 4);
  signal xfypt : real;
  signal tbnne : std_logic;
begin
  ejum : entity work.fmqd
    port map (guhn => tbnne, qicr => xfypt, zlapthj => tbnne, zjzbiyk => tbnqhxzjcv);
  hdjukkpbs : entity work.fmqd
    port map (guhn => tbnne, qicr => utphupo, zlapthj => ic, zjzbiyk => esf);
  fmfalkw : entity work.fmqd
    port map (guhn => tbnne, qicr => xfypt, zlapthj => tbnne, zjzbiyk => wh);
  
  -- Single-driven assignments
  xfypt <= 8#63707.4_1_2#;
  utphupo <= 2#001.00010#;
  msjacahe <= 'g';
  
  -- Multi-driven assignments
  tbnne <= '0';
  esf <= "L0X";
  tbnne <= 'X';
end qvz;

library ieee;
use ieee.std_logic_1164.all;

entity glqetsoa is
  port (iashs : linkage std_logic);
end glqetsoa;

library ieee;
use ieee.std_logic_1164.all;

architecture ixpeqzrg of glqetsoa is
  signal h : std_logic_vector(2 to 4);
  signal najbc : real;
  signal cruq : std_logic;
  signal jto : character;
  signal rsyxwdbgx : character;
  signal asaswow : character;
  signal bt : time;
begin
  rpfd : entity work.xfs
    port map (gjuq => bt, msjacahe => asaswow);
  ocnyfb : entity work.xfs
    port map (gjuq => bt, msjacahe => rsyxwdbgx);
  ee : entity work.xfs
    port map (gjuq => bt, msjacahe => jto);
  hax : entity work.fmqd
    port map (guhn => cruq, qicr => najbc, zlapthj => cruq, zjzbiyk => h);
  
  -- Multi-driven assignments
  cruq <= 'L';
  cruq <= '-';
  cruq <= '0';
end ixpeqzrg;

library ieee;
use ieee.std_logic_1164.all;

entity mwzt is
  port (lhsotdhuhi : in std_logic; cwfauvo : inout real);
end mwzt;

library ieee;
use ieee.std_logic_1164.all;

architecture cs of mwzt is
  signal bwnwzkyfl : std_logic_vector(2 to 4);
  signal zrtffx : std_logic;
  signal jshgaugq : real;
  signal edoy : std_logic;
  signal bry : std_logic_vector(2 to 4);
  signal ad : std_logic;
begin
  e : entity work.fmqd
    port map (guhn => ad, qicr => cwfauvo, zlapthj => ad, zjzbiyk => bry);
  rqer : entity work.fmqd
    port map (guhn => edoy, qicr => jshgaugq, zlapthj => zrtffx, zjzbiyk => bwnwzkyfl);
  
  -- Multi-driven assignments
  ad <= 'U';
  bwnwzkyfl <= ('L', '-', 'L');
  ad <= 'L';
end cs;



-- Seed after: 16981290943247802977,14652815260262078753
