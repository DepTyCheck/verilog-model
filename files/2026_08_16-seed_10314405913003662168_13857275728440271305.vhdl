-- Seed: 10314405913003662168,13857275728440271305

entity pj is
  port (jddmh : out character; pxnmbwvrmz : linkage real; ouqmgqnna : buffer integer);
end pj;

architecture r of pj is
  
begin
  
end r;

entity cwcq is
  port (newydlmv : linkage bit_vector(1 to 3); hbzc : linkage integer);
end cwcq;

architecture ecbgit of cwcq is
  signal yatz : integer;
  signal wnitxokngm : real;
  signal h : character;
  signal zly : integer;
  signal w : real;
  signal ahrcttqnhh : character;
begin
  zpveytnsg : entity work.pj
    port map (jddmh => ahrcttqnhh, pxnmbwvrmz => w, ouqmgqnna => zly);
  sccydzrlby : entity work.pj
    port map (jddmh => h, pxnmbwvrmz => wnitxokngm, ouqmgqnna => yatz);
end ecbgit;

library ieee;
use ieee.std_logic_1164.all;

entity nyg is
  port (hvzdumozff : linkage boolean; ac : out std_logic);
end nyg;

architecture erxuonxq of nyg is
  signal uk : integer;
  signal nanmpq : real;
  signal e : character;
  signal lyhed : integer;
  signal cj : bit_vector(1 to 3);
begin
  ch : entity work.cwcq
    port map (newydlmv => cj, hbzc => lyhed);
  tqj : entity work.pj
    port map (jddmh => e, pxnmbwvrmz => nanmpq, ouqmgqnna => uk);
  
  -- Multi-driven assignments
  ac <= '1';
  ac <= ac;
end erxuonxq;

entity neppt is
  port (bih : linkage integer);
end neppt;

library ieee;
use ieee.std_logic_1164.all;

architecture ycgjwp of neppt is
  signal bhlivru : std_logic;
  signal jysa : boolean;
begin
  boxm : entity work.nyg
    port map (hvzdumozff => jysa, ac => bhlivru);
  
  -- Multi-driven assignments
  bhlivru <= bhlivru;
  bhlivru <= bhlivru;
end ycgjwp;



-- Seed after: 3518182775315825215,13857275728440271305
