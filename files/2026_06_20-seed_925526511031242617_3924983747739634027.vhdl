-- Seed: 925526511031242617,3924983747739634027

entity zmbqsrt is
  port (cacwf : out boolean_vector(3 downto 0); zu : buffer boolean_vector(2 downto 4); mbrmh : inout time; mhox : buffer real_vector(1 downto 0));
end zmbqsrt;

architecture synyu of zmbqsrt is
  
begin
  -- Single-driven assignments
  mhox <= (8#60.3_7_0_2#, 3_0_4.21133);
  mbrmh <= 3.0_0_4 us;
end synyu;

entity muzbnr is
  port (ikhhx : in real; jmmjodjptc : in real);
end muzbnr;

architecture tkg of muzbnr is
  signal njtvrn : real_vector(1 downto 0);
  signal psmlphi : time;
  signal hn : boolean_vector(2 downto 4);
  signal bgqjkzza : boolean_vector(3 downto 0);
  signal vnrye : real_vector(1 downto 0);
  signal uwztndiz : time;
  signal bawukpriye : boolean_vector(2 downto 4);
  signal lzoqfl : boolean_vector(3 downto 0);
begin
  hpbzmsq : entity work.zmbqsrt
    port map (cacwf => lzoqfl, zu => bawukpriye, mbrmh => uwztndiz, mhox => vnrye);
  ljg : entity work.zmbqsrt
    port map (cacwf => bgqjkzza, zu => hn, mbrmh => psmlphi, mhox => njtvrn);
end tkg;

entity oeiwdpufs is
  port (cezvrsjq : linkage integer; bhxlw : inout integer; fjph : out real; wxejabuugn : inout time);
end oeiwdpufs;

architecture wej of oeiwdpufs is
  signal v : real;
begin
  joftdpmbq : entity work.muzbnr
    port map (ikhhx => v, jmmjodjptc => fjph);
  
  -- Single-driven assignments
  wxejabuugn <= 8#0201.4_5_2_7_1# ps;
  bhxlw <= 0;
  fjph <= 2.4_2_1_1_1;
end wej;



-- Seed after: 2179002910549975930,3924983747739634027
