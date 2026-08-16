-- Seed: 12118974008394543227,13857275728440271305

entity lyl is
  port (xndrm : out time_vector(3 to 4));
end lyl;

architecture bmntsrgvgp of lyl is
  
begin
  -- Single-driven assignments
  xndrm <= (2 fs, 2#00010.1_0_0# ns);
end bmntsrgvgp;

library ieee;
use ieee.std_logic_1164.all;

entity pqrnveikd is
  port (qcxhq : out time_vector(1 to 2); wsvnujuwy : linkage std_logic_vector(1 downto 1); o : linkage std_logic_vector(4 to 3); mjijs : in time);
end pqrnveikd;

architecture qjvpbwwmk of pqrnveikd is
  signal htvyrjv : time_vector(3 to 4);
begin
  qczfgjqum : entity work.lyl
    port map (xndrm => htvyrjv);
end qjvpbwwmk;

entity vppdhzt is
  port (uno : linkage real);
end vppdhzt;

architecture vqlhvoxplo of vppdhzt is
  signal bv : time_vector(3 to 4);
  signal gigqbwzw : time_vector(3 to 4);
  signal emkpxwd : time_vector(3 to 4);
begin
  dwdsndeifl : entity work.lyl
    port map (xndrm => emkpxwd);
  e : entity work.lyl
    port map (xndrm => gigqbwzw);
  kgfnl : entity work.lyl
    port map (xndrm => bv);
end vqlhvoxplo;

library ieee;
use ieee.std_logic_1164.all;

entity jypo is
  port (wvpx : buffer std_logic; g : inout boolean; agq : linkage time);
end jypo;

library ieee;
use ieee.std_logic_1164.all;

architecture aehwdlob of jypo is
  signal uc : time_vector(3 to 4);
  signal hybvy : time_vector(3 to 4);
  signal aqlaiprsl : time;
  signal lu : std_logic_vector(4 to 3);
  signal esvbqzuyhi : std_logic_vector(1 downto 1);
  signal tlzrb : time_vector(1 to 2);
begin
  nwjjji : entity work.pqrnveikd
    port map (qcxhq => tlzrb, wsvnujuwy => esvbqzuyhi, o => lu, mjijs => aqlaiprsl);
  iqhrunfl : entity work.lyl
    port map (xndrm => hybvy);
  uthuvdvuuu : entity work.lyl
    port map (xndrm => uc);
  
  -- Single-driven assignments
  aqlaiprsl <= aqlaiprsl;
  
  -- Multi-driven assignments
  wvpx <= wvpx;
  wvpx <= 'H';
  wvpx <= wvpx;
end aehwdlob;



-- Seed after: 8642774169723729071,13857275728440271305
