-- Seed: 4279454570295160455,11460338385561364835



entity bcabnjueb is
  port (tnamepeted : inout integer; fgykbdoobc : in time; qwytyarux : in real);
end bcabnjueb;



architecture lka of bcabnjueb is
  
begin
  
end lka;



entity cutu is
  port (xjshpzmixp : buffer integer);
end cutu;



architecture xi of cutu is
  signal gbrkkzrl : real;
  signal ufhwl : time;
  signal nvdrjyehjx : integer;
begin
  rvinwmw : entity work.bcabnjueb
    port map (tnamepeted => nvdrjyehjx, fgykbdoobc => ufhwl, qwytyarux => gbrkkzrl);
  iwtphcmon : entity work.bcabnjueb
    port map (tnamepeted => xjshpzmixp, fgykbdoobc => ufhwl, qwytyarux => gbrkkzrl);
end xi;



entity czp is
  port (w : inout integer; fteogvob : inout integer; pwmyzbmmn : buffer time);
end czp;



architecture ozfkr of czp is
  signal meornomnef : integer;
  signal qhvpj : integer;
begin
  avgp : entity work.cutu
    port map (xjshpzmixp => qhvpj);
  aiisn : entity work.cutu
    port map (xjshpzmixp => meornomnef);
end ozfkr;



-- Seed after: 18036338601193778330,11460338385561364835
