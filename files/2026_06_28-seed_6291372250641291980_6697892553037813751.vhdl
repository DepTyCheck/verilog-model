-- Seed: 6291372250641291980,6697892553037813751

entity xd is
  port (iaxzhz : buffer boolean; xnmp : in time);
end xd;

architecture rrlntuq of xd is
  
begin
  -- Single-driven assignments
  iaxzhz <= TRUE;
end rrlntuq;

entity zrzn is
  port (nfdktebh : in real);
end zrzn;

architecture gm of zrzn is
  signal z : boolean;
  signal ylxqry : time;
  signal zegxr : boolean;
begin
  nxu : entity work.xd
    port map (iaxzhz => zegxr, xnmp => ylxqry);
  xxs : entity work.xd
    port map (iaxzhz => z, xnmp => ylxqry);
  
  -- Single-driven assignments
  ylxqry <= 16#E027B.0_0_D_C# ns;
end gm;

entity ntfftfa is
  port (yecgflo : inout integer; tcfynvvug : out character; kxi : out real);
end ntfftfa;

architecture mwtlp of ntfftfa is
  signal edgwqrjya : boolean;
  signal yh : time;
  signal tyktrh : boolean;
begin
  ivzo : entity work.xd
    port map (iaxzhz => tyktrh, xnmp => yh);
  pmldyy : entity work.xd
    port map (iaxzhz => edgwqrjya, xnmp => yh);
end mwtlp;

entity lpevdskct is
  port (ykq : linkage character; jzpvlil : inout real; fwodbdoktt : buffer boolean_vector(3 to 0));
end lpevdskct;

architecture wkhns of lpevdskct is
  signal crgbmtlxjj : real;
  signal nu : character;
  signal l : integer;
  signal rdqwq : boolean;
  signal fndnpxunk : real;
  signal cpaibvqtml : time;
  signal izm : boolean;
begin
  shdcdyw : entity work.xd
    port map (iaxzhz => izm, xnmp => cpaibvqtml);
  nhhbtizf : entity work.zrzn
    port map (nfdktebh => fndnpxunk);
  q : entity work.xd
    port map (iaxzhz => rdqwq, xnmp => cpaibvqtml);
  fw : entity work.ntfftfa
    port map (yecgflo => l, tcfynvvug => nu, kxi => crgbmtlxjj);
  
  -- Single-driven assignments
  fwodbdoktt <= (others => TRUE);
  fndnpxunk <= 8#3_3_1_2_5.1#;
  cpaibvqtml <= 3_1_4_2_2 fs;
  jzpvlil <= 3.1_1_4_2;
end wkhns;



-- Seed after: 15561007153264554020,6697892553037813751
