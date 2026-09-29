-- Seed: 4657722637223811824,10940991575366938685

entity funqfgkc is
  port (weiemxavj : in time_vector(2 downto 2));
end funqfgkc;

architecture zhnnmzdepz of funqfgkc is
  
begin
  
end zhnnmzdepz;

entity p is
  port (qc : inout time; hgn : linkage integer; mcyfvetsjl : buffer time_vector(4 downto 2));
end p;

architecture xxsv of p is
  signal aqnknc : time_vector(2 downto 2);
begin
  fxf : entity work.funqfgkc
    port map (weiemxavj => aqnknc);
  wsdndmhtaw : entity work.funqfgkc
    port map (weiemxavj => aqnknc);
  gy : entity work.funqfgkc
    port map (weiemxavj => aqnknc);
end xxsv;

entity wtvkgeygds is
  port (yt : linkage real);
end wtvkgeygds;

architecture uftghwgrbp of wtvkgeygds is
  signal tnhelp : time_vector(2 downto 2);
  signal fumxprqmy : time_vector(4 downto 2);
  signal cetspt : integer;
  signal ztya : time;
  signal xiazc : time_vector(2 downto 2);
begin
  ol : entity work.funqfgkc
    port map (weiemxavj => xiazc);
  gkm : entity work.p
    port map (qc => ztya, hgn => cetspt, mcyfvetsjl => fumxprqmy);
  tlzrc : entity work.funqfgkc
    port map (weiemxavj => tnhelp);
  vwl : entity work.funqfgkc
    port map (weiemxavj => tnhelp);
  
  -- Single-driven assignments
  tnhelp <= xiazc;
  xiazc <= xiazc;
end uftghwgrbp;

entity hwdzfc is
  port (jchyl : out bit_vector(0 to 0); awj : in boolean; jfcdp : in integer);
end hwdzfc;

architecture kunnlle of hwdzfc is
  signal foydyrydrs : time_vector(4 downto 2);
  signal bwktppv : integer;
  signal lv : time;
  signal vtptvpgzd : time_vector(2 downto 2);
  signal m : time_vector(2 downto 2);
begin
  tfjii : entity work.funqfgkc
    port map (weiemxavj => m);
  vfuln : entity work.funqfgkc
    port map (weiemxavj => vtptvpgzd);
  bs : entity work.p
    port map (qc => lv, hgn => bwktppv, mcyfvetsjl => foydyrydrs);
  
  -- Single-driven assignments
  m <= (others => 4 sec);
  vtptvpgzd <= (others => 16#4_E_5# us);
end kunnlle;



-- Seed after: 1166528232768178623,10940991575366938685
