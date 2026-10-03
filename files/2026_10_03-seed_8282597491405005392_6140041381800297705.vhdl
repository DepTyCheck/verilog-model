-- Seed: 8282597491405005392,6140041381800297705

entity qfjbga is
  port (ablwlu : in bit_vector(1 to 3); yyqnmwv : inout integer; gtwxncl : in time);
end qfjbga;

architecture iv of qfjbga is
  
begin
  -- Single-driven assignments
  yyqnmwv <= 2;
end iv;

entity iogpbld is
  port (qbttrnw : inout integer; h : in bit);
end iogpbld;

architecture uvphlymvbx of iogpbld is
  
begin
  -- Single-driven assignments
  qbttrnw <= qbttrnw;
end uvphlymvbx;

entity owskctkcda is
  port (jeiyocn : linkage integer; drtjrjd : inout real_vector(1 to 0); ps : inout time);
end owskctkcda;

architecture r of owskctkcda is
  signal kqmv : integer;
  signal e : integer;
  signal fgb : bit;
  signal qmagzui : integer;
  signal seqvzn : time;
  signal vfshv : integer;
  signal aauvmv : bit_vector(1 to 3);
begin
  aeanjifpre : entity work.qfjbga
    port map (ablwlu => aauvmv, yyqnmwv => vfshv, gtwxncl => seqvzn);
  hvtwj : entity work.iogpbld
    port map (qbttrnw => qmagzui, h => fgb);
  xiujjwyj : entity work.qfjbga
    port map (ablwlu => aauvmv, yyqnmwv => e, gtwxncl => ps);
  rg : entity work.qfjbga
    port map (ablwlu => aauvmv, yyqnmwv => kqmv, gtwxncl => ps);
  
  -- Single-driven assignments
  fgb <= fgb;
  drtjrjd <= (others => 0.0);
  ps <= ps;
end r;



-- Seed after: 11845336663811296641,6140041381800297705
