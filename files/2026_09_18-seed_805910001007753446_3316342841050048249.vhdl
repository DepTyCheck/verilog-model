-- Seed: 805910001007753446,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity ulhk is
  port (xpbypgl : out time; bgdeia : out time; lnmrxowq : out time; zdq : in std_logic_vector(3 to 2));
end ulhk;

architecture gggw of ulhk is
  
begin
  -- Single-driven assignments
  lnmrxowq <= 30.0_1_4_4_2 fs;
  xpbypgl <= 1 fs;
  bgdeia <= 16#1_A_3_5.0_B_7_6# ps;
end gggw;

library ieee;
use ieee.std_logic_1164.all;

entity aku is
  port (gyon : linkage std_logic; ybkyq : in time; pfnh : out std_logic; ubsqz : buffer time);
end aku;

library ieee;
use ieee.std_logic_1164.all;

architecture wgzwu of aku is
  signal cl : std_logic_vector(3 to 2);
  signal gw : time;
  signal ilea : time;
  signal cuh : time;
  signal leabrlq : std_logic_vector(3 to 2);
  signal jsi : time;
  signal ogtrycjydm : time;
  signal svzosljvsd : std_logic_vector(3 to 2);
  signal rq : time;
  signal gxkx : time;
  signal kudmpgcvq : time;
begin
  pnd : entity work.ulhk
    port map (xpbypgl => kudmpgcvq, bgdeia => gxkx, lnmrxowq => rq, zdq => svzosljvsd);
  ytstngvzap : entity work.ulhk
    port map (xpbypgl => ubsqz, bgdeia => ogtrycjydm, lnmrxowq => jsi, zdq => leabrlq);
  zxhwtz : entity work.ulhk
    port map (xpbypgl => cuh, bgdeia => ilea, lnmrxowq => gw, zdq => cl);
  
  -- Multi-driven assignments
  leabrlq <= "";
  pfnh <= 'H';
  pfnh <= '0';
  svzosljvsd <= "";
end wgzwu;

library ieee;
use ieee.std_logic_1164.all;

entity eqstlof is
  port (nrxjcgme : linkage string(3 to 5); b : linkage std_logic; rjgv : out time; rvxk : buffer time);
end eqstlof;

library ieee;
use ieee.std_logic_1164.all;

architecture j of eqstlof is
  signal iguavvhvo : time;
  signal jtnbkgu : std_logic;
  signal strfzwpah : time;
  signal gritkije : time;
  signal vtuxu : time;
  signal cqp : std_logic_vector(3 to 2);
  signal bqfhcom : time;
  signal oj : time;
  signal wglbwvsza : time;
  signal kpue : std_logic;
begin
  szjboxf : entity work.aku
    port map (gyon => kpue, ybkyq => wglbwvsza, pfnh => kpue, ubsqz => oj);
  ew : entity work.ulhk
    port map (xpbypgl => wglbwvsza, bgdeia => bqfhcom, lnmrxowq => rvxk, zdq => cqp);
  fm : entity work.ulhk
    port map (xpbypgl => vtuxu, bgdeia => rjgv, lnmrxowq => gritkije, zdq => cqp);
  iiqhhbh : entity work.aku
    port map (gyon => b, ybkyq => strfzwpah, pfnh => jtnbkgu, ubsqz => iguavvhvo);
  
  -- Single-driven assignments
  strfzwpah <= rvxk;
  
  -- Multi-driven assignments
  cqp <= (others => '0');
end j;

entity op is
  port (uymphbm : out bit_vector(2 downto 3); ryxuhrh : linkage time; mio : inout time);
end op;

library ieee;
use ieee.std_logic_1164.all;

architecture qtlmmbgytx of op is
  signal edt : time;
  signal ohuls : std_logic;
  signal zypqkzjvvk : string(3 to 5);
begin
  zsukxip : entity work.eqstlof
    port map (nrxjcgme => zypqkzjvvk, b => ohuls, rjgv => edt, rvxk => mio);
  
  -- Single-driven assignments
  uymphbm <= uymphbm;
  
  -- Multi-driven assignments
  ohuls <= ohuls;
  ohuls <= ohuls;
  ohuls <= '1';
end qtlmmbgytx;



-- Seed after: 6235616511057335434,3316342841050048249
