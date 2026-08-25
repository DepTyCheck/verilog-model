-- Seed: 7656484234313514436,13501862637168280927

entity ayx is
  port (q : out boolean_vector(0 to 2); vlwsew : out boolean);
end ayx;

architecture erkfl of ayx is
  
begin
  -- Single-driven assignments
  q <= (FALSE, FALSE, FALSE);
  vlwsew <= FALSE;
end erkfl;

entity pgmfhmyl is
  port (ycmw : linkage integer; kdbioolth : buffer boolean_vector(1 to 0); cgfbpcks : inout severity_level);
end pgmfhmyl;

architecture vj of pgmfhmyl is
  signal bqob : boolean;
  signal hvst : boolean_vector(0 to 2);
  signal s : boolean;
  signal bwxnogtgpq : boolean_vector(0 to 2);
  signal nudqcy : boolean;
  signal jxhhxqu : boolean_vector(0 to 2);
  signal xwu : boolean;
  signal am : boolean_vector(0 to 2);
begin
  jpfsmaor : entity work.ayx
    port map (q => am, vlwsew => xwu);
  m : entity work.ayx
    port map (q => jxhhxqu, vlwsew => nudqcy);
  wbltkqvvem : entity work.ayx
    port map (q => bwxnogtgpq, vlwsew => s);
  r : entity work.ayx
    port map (q => hvst, vlwsew => bqob);
  
  -- Single-driven assignments
  cgfbpcks <= cgfbpcks;
  kdbioolth <= kdbioolth;
end vj;



-- Seed after: 14110315919188504038,13501862637168280927
