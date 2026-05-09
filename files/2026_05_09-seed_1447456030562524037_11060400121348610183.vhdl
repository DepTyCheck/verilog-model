-- Seed: 1447456030562524037,11060400121348610183



entity sumlvbnhi is
  port (coz : inout time; fwlc : out time);
end sumlvbnhi;



architecture prs of sumlvbnhi is
  
begin
  
end prs;



entity xoivzk is
  port (vz : buffer real; xqzpyfue : linkage integer; adq : in time);
end xoivzk;



architecture gzvdgzol of xoivzk is
  signal guuxjw : time;
  signal uap : time;
  signal lgosfn : time;
  signal idsmlwfmb : time;
begin
  jafcpl : entity work.sumlvbnhi
    port map (coz => idsmlwfmb, fwlc => lgosfn);
  eqz : entity work.sumlvbnhi
    port map (coz => uap, fwlc => guuxjw);
end gzvdgzol;



entity zoxrb is
  port (qxtjwi : inout real; jyiliv : inout integer; pccouggbtc : linkage severity_level);
end zoxrb;



architecture tlt of zoxrb is
  signal sxccc : time;
  signal ukjhhrsxm : time;
  signal tcaypuv : real;
  signal scvnyeeezp : time;
  signal scsmpps : real;
begin
  wempfddbg : entity work.xoivzk
    port map (vz => scsmpps, xqzpyfue => jyiliv, adq => scvnyeeezp);
  omfdvr : entity work.xoivzk
    port map (vz => tcaypuv, xqzpyfue => jyiliv, adq => ukjhhrsxm);
  nj : entity work.xoivzk
    port map (vz => qxtjwi, xqzpyfue => jyiliv, adq => scvnyeeezp);
  bfyrzq : entity work.sumlvbnhi
    port map (coz => scvnyeeezp, fwlc => sxccc);
end tlt;



entity tl is
  port (niogot : linkage integer; xbitirroin : linkage time; vgbpanvf : out time; agltzaqmz : linkage integer);
end tl;



architecture qejfmryzf of tl is
  signal kogtaq : real;
begin
  ckytonz : entity work.xoivzk
    port map (vz => kogtaq, xqzpyfue => agltzaqmz, adq => vgbpanvf);
end qejfmryzf;



-- Seed after: 12893914544982096280,11060400121348610183
