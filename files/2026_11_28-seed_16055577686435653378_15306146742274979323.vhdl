-- Seed: 16055577686435653378,15306146742274979323

entity vpscz is
  port (dovfpz : in boolean_vector(3 to 2); dfdeuwts : buffer string(4 downto 2); xxfbd : linkage time; tsn : in real);
end vpscz;

architecture xrb of vpscz is
  
begin
  -- Single-driven assignments
  dfdeuwts <= dfdeuwts;
end xrb;

entity g is
  port (uojizerj : linkage real; suxexwn : linkage severity_level; vhvuhggcwf : in time_vector(1 to 3); lwphsw : linkage integer);
end g;

architecture m of g is
  signal z : time;
  signal h : string(4 downto 2);
  signal wqyl : boolean_vector(3 to 2);
  signal dhfikclv : real;
  signal ykdiz : time;
  signal hhzwyydd : string(4 downto 2);
  signal rh : boolean_vector(3 to 2);
begin
  zglzp : entity work.vpscz
    port map (dovfpz => rh, dfdeuwts => hhzwyydd, xxfbd => ykdiz, tsn => dhfikclv);
  clnkxwkb : entity work.vpscz
    port map (dovfpz => wqyl, dfdeuwts => h, xxfbd => z, tsn => dhfikclv);
  
  -- Single-driven assignments
  dhfikclv <= 2#1.0#;
  wqyl <= (others => TRUE);
  rh <= (others => TRUE);
end m;



-- Seed after: 1907203159679802375,15306146742274979323
