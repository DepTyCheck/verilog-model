-- Seed: 5551737194949868168,5472058987609252853

entity mnpp is
  port (bhceetf : out boolean; umsynrfmvo : inout bit_vector(3 to 1); gfzaj : out real);
end mnpp;

architecture qjqds of mnpp is
  
begin
  -- Single-driven assignments
  umsynrfmvo <= (others => '0');
  bhceetf <= TRUE;
  gfzaj <= 2#011.0_1_1#;
end qjqds;

entity hxjgyftok is
  port (ge : buffer time);
end hxjgyftok;

architecture jqpaxwf of hxjgyftok is
  signal vfaejedd : real;
  signal hzjryh : bit_vector(3 to 1);
  signal bnxkwvpls : boolean;
  signal sp : real;
  signal opmlfyl : bit_vector(3 to 1);
  signal beiw : boolean;
begin
  tik : entity work.mnpp
    port map (bhceetf => beiw, umsynrfmvo => opmlfyl, gfzaj => sp);
  kgnahl : entity work.mnpp
    port map (bhceetf => bnxkwvpls, umsynrfmvo => hzjryh, gfzaj => vfaejedd);
  
  -- Single-driven assignments
  ge <= 4 min;
end jqpaxwf;

library ieee;
use ieee.std_logic_1164.all;

entity mnjqkeske is
  port (oumvzhra : out std_logic; p : linkage time);
end mnjqkeske;

architecture hnvlgvwyva of mnjqkeske is
  signal en : real;
  signal qdoel : bit_vector(3 to 1);
  signal rtdenkhjv : boolean;
  signal q : time;
  signal djppzt : real;
  signal gmqkspnqp : bit_vector(3 to 1);
  signal ozed : boolean;
  signal gzfm : real;
  signal tqyfakwys : bit_vector(3 to 1);
  signal ccgkcmn : boolean;
begin
  gnxhf : entity work.mnpp
    port map (bhceetf => ccgkcmn, umsynrfmvo => tqyfakwys, gfzaj => gzfm);
  skfwk : entity work.mnpp
    port map (bhceetf => ozed, umsynrfmvo => gmqkspnqp, gfzaj => djppzt);
  uhbesafoqr : entity work.hxjgyftok
    port map (ge => q);
  aljsb : entity work.mnpp
    port map (bhceetf => rtdenkhjv, umsynrfmvo => qdoel, gfzaj => en);
  
  -- Multi-driven assignments
  oumvzhra <= 'W';
  oumvzhra <= 'W';
  oumvzhra <= '1';
end hnvlgvwyva;



-- Seed after: 2302746613741572095,5472058987609252853
