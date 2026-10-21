-- Seed: 11073675517610956607,17219977430298397867

entity kc is
  port (ykeihybu : in time_vector(4 downto 0); q : inout integer; mms : out time; ap : in character);
end kc;

architecture kegyaish of kc is
  
begin
  -- Single-driven assignments
  mms <= 16#C_7_B_F# ms;
  q <= 8#5#;
end kegyaish;

library ieee;
use ieee.std_logic_1164.all;

entity ihzaopgi is
  port (mqejabyeyf : inout integer; eoza : buffer real; vkhuxfa : in std_logic; mtwktaa : linkage std_logic_vector(1 to 4));
end ihzaopgi;

architecture mlfztxwqeq of ihzaopgi is
  signal w : time;
  signal pmz : time_vector(4 downto 0);
  signal uzmrg : time;
  signal gmo : integer;
  signal k : time_vector(4 downto 0);
  signal es : time;
  signal fxjlmym : integer;
  signal evshyok : character;
  signal hsverivlwf : time;
  signal wjp : integer;
  signal jc : time_vector(4 downto 0);
begin
  csojzt : entity work.kc
    port map (ykeihybu => jc, q => wjp, mms => hsverivlwf, ap => evshyok);
  pwschsq : entity work.kc
    port map (ykeihybu => jc, q => fxjlmym, mms => es, ap => evshyok);
  flqbhhz : entity work.kc
    port map (ykeihybu => k, q => gmo, mms => uzmrg, ap => evshyok);
  ycdaahzdl : entity work.kc
    port map (ykeihybu => pmz, q => mqejabyeyf, mms => w, ap => evshyok);
end mlfztxwqeq;



-- Seed after: 9271711704677176737,17219977430298397867
