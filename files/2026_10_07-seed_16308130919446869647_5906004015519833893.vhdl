-- Seed: 16308130919446869647,5906004015519833893

entity obn is
  port (umkhry : out integer; izfltmh : buffer severity_level; shz : in real; nvxjlmfgv : linkage bit_vector(1 to 2));
end obn;

architecture hfsjdexmf of obn is
  
begin
  -- Single-driven assignments
  umkhry <= 8#7136#;
  izfltmh <= WARNING;
end hfsjdexmf;

entity jmkmrlnwqu is
  port (gkrtsny : inout bit_vector(4 to 3));
end jmkmrlnwqu;

architecture bxmvg of jmkmrlnwqu is
  signal bly : bit_vector(1 to 2);
  signal jlcvy : severity_level;
  signal oo : integer;
  signal hhlftmlvv : bit_vector(1 to 2);
  signal fvjrtfskb : severity_level;
  signal ktl : integer;
  signal bdwzcqyfa : bit_vector(1 to 2);
  signal bdcrfqooz : real;
  signal qbfxzzaxaj : severity_level;
  signal edckxrzcjx : integer;
begin
  mhur : entity work.obn
    port map (umkhry => edckxrzcjx, izfltmh => qbfxzzaxaj, shz => bdcrfqooz, nvxjlmfgv => bdwzcqyfa);
  skq : entity work.obn
    port map (umkhry => ktl, izfltmh => fvjrtfskb, shz => bdcrfqooz, nvxjlmfgv => hhlftmlvv);
  cnh : entity work.obn
    port map (umkhry => oo, izfltmh => jlcvy, shz => bdcrfqooz, nvxjlmfgv => bly);
  
  -- Single-driven assignments
  gkrtsny <= gkrtsny;
  bdcrfqooz <= bdcrfqooz;
end bxmvg;

library ieee;
use ieee.std_logic_1164.all;

entity ylf is
  port (dtswrudly : in integer; s : buffer std_logic);
end ylf;

architecture zeulhc of ylf is
  signal uqnbslah : bit_vector(1 to 2);
  signal u : severity_level;
  signal ssfclvuwf : integer;
  signal ox : bit_vector(1 to 2);
  signal sd : real;
  signal zgkc : severity_level;
  signal llinnfbbwn : integer;
  signal v : bit_vector(1 to 2);
  signal wrhhc : real;
  signal kpakjykjyh : severity_level;
  signal nnsiuofzg : integer;
  signal iloztloeg : bit_vector(1 to 2);
  signal ksaec : real;
  signal vyzpmaotfq : severity_level;
  signal ofspur : integer;
begin
  vtvbwzex : entity work.obn
    port map (umkhry => ofspur, izfltmh => vyzpmaotfq, shz => ksaec, nvxjlmfgv => iloztloeg);
  seavfryvru : entity work.obn
    port map (umkhry => nnsiuofzg, izfltmh => kpakjykjyh, shz => wrhhc, nvxjlmfgv => v);
  uswzhenn : entity work.obn
    port map (umkhry => llinnfbbwn, izfltmh => zgkc, shz => sd, nvxjlmfgv => ox);
  kiob : entity work.obn
    port map (umkhry => ssfclvuwf, izfltmh => u, shz => sd, nvxjlmfgv => uqnbslah);
  
  -- Single-driven assignments
  ksaec <= ksaec;
  wrhhc <= ksaec;
  sd <= ksaec;
  
  -- Multi-driven assignments
  s <= s;
end zeulhc;



-- Seed after: 13857872574280826681,5906004015519833893
