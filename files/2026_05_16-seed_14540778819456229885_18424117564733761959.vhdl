-- Seed: 14540778819456229885,18424117564733761959



entity b is
  port (okbfs : buffer real);
end b;



architecture nonpgbxqx of b is
  
begin
  
end nonpgbxqx;



entity i is
  port (vvcejpz : linkage bit; ywk : linkage real; bokwrapk : in real);
end i;



architecture qjgygj of i is
  signal bikfpnrye : real;
  signal sdsmqllat : real;
  signal kbdzahvr : real;
begin
  lnwubsrd : entity work.b
    port map (okbfs => kbdzahvr);
  qlrophuvo : entity work.b
    port map (okbfs => sdsmqllat);
  igqtjtjkl : entity work.b
    port map (okbfs => bikfpnrye);
end qjgygj;



entity yfj is
  port (qv : inout integer; hzkgm : linkage time);
end yfj;



architecture kjby of yfj is
  signal lkzbpoyz : real;
begin
  iufuajgzsb : entity work.b
    port map (okbfs => lkzbpoyz);
end kjby;



entity wyaz is
  port (cfgststr : in time; cyzs : in time; yfgjcgt : out integer);
end wyaz;



architecture tbwgeyvz of wyaz is
  signal x : real;
  signal ioeiofz : real;
  signal oubosabg : bit;
begin
  bc : entity work.i
    port map (vvcejpz => oubosabg, ywk => ioeiofz, bokwrapk => x);
  anta : entity work.yfj
    port map (qv => yfgjcgt, hzkgm => cyzs);
  yn : entity work.b
    port map (okbfs => ioeiofz);
end tbwgeyvz;



-- Seed after: 8466178737774198274,18424117564733761959
