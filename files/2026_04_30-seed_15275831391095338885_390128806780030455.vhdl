-- Seed: 15275831391095338885,390128806780030455



entity evzjjvyio is
  port (yserc : buffer time; luknwxo : in time; mg : inout boolean; uxusbogyp : out integer);
end evzjjvyio;



architecture puj of evzjjvyio is
  
begin
  
end puj;



entity bodi is
  port (xq : buffer time; neeqh : inout integer);
end bodi;



architecture lqo of bodi is
  signal jrw : integer;
  signal u : boolean;
  signal vbknrwx : boolean;
  signal dthq : time;
  signal brx : integer;
  signal kusketbwu : boolean;
  signal avvqpr : integer;
  signal houhwcd : boolean;
  signal gwxtqcsq : time;
  signal ucvalwm : time;
begin
  pdc : entity work.evzjjvyio
    port map (yserc => ucvalwm, luknwxo => gwxtqcsq, mg => houhwcd, uxusbogyp => avvqpr);
  uvu : entity work.evzjjvyio
    port map (yserc => gwxtqcsq, luknwxo => ucvalwm, mg => kusketbwu, uxusbogyp => brx);
  gg : entity work.evzjjvyio
    port map (yserc => dthq, luknwxo => xq, mg => vbknrwx, uxusbogyp => neeqh);
  ibydqjeduy : entity work.evzjjvyio
    port map (yserc => xq, luknwxo => dthq, mg => u, uxusbogyp => jrw);
end lqo;

library ieee;
use ieee.std_logic_1164.all;

entity qqkfu is
  port (zw : out real; zdovaqtqwq : linkage integer; xjncgfz : buffer std_logic; w : linkage time);
end qqkfu;



architecture dhytu of qqkfu is
  signal jd : integer;
  signal hkosqk : boolean;
  signal vqbz : time;
  signal vmamvummdg : integer;
  signal y : boolean;
  signal ozsuoetsw : time;
  signal nfwkkl : integer;
  signal fgz : boolean;
  signal ngttbr : time;
  signal emdojznogz : time;
  signal htnhkle : integer;
  signal ykdcbgbm : time;
begin
  ytlqfpcw : entity work.bodi
    port map (xq => ykdcbgbm, neeqh => htnhkle);
  fdc : entity work.evzjjvyio
    port map (yserc => emdojznogz, luknwxo => ngttbr, mg => fgz, uxusbogyp => nfwkkl);
  zjwrdprbnv : entity work.evzjjvyio
    port map (yserc => ngttbr, luknwxo => ozsuoetsw, mg => y, uxusbogyp => vmamvummdg);
  iqo : entity work.evzjjvyio
    port map (yserc => vqbz, luknwxo => vqbz, mg => hkosqk, uxusbogyp => jd);
end dhytu;



entity y is
  port (xwchjtgwgk : buffer time; b : in real; gmdwh : out real; bwqpacv : buffer time);
end y;



architecture ieopafjz of y is
  signal pqbd : integer;
  signal cswu : boolean;
  signal nyuhik : time;
begin
  kair : entity work.evzjjvyio
    port map (yserc => nyuhik, luknwxo => bwqpacv, mg => cswu, uxusbogyp => pqbd);
end ieopafjz;



-- Seed after: 3186738787373622163,390128806780030455
