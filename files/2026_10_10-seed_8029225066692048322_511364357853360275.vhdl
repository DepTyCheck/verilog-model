-- Seed: 8029225066692048322,511364357853360275

entity qeof is
  port (ulloqurnl : in time; yeemadtpg : out real; qwugpopync : linkage boolean; nkhgk : linkage bit);
end qeof;

architecture c of qeof is
  
begin
  -- Single-driven assignments
  yeemadtpg <= 234.3_4_0_1_0;
end c;

entity sihtqox is
  port (jzyyibc : linkage integer; y : inout integer_vector(1 to 2));
end sihtqox;

architecture l of sihtqox is
  signal g : bit;
  signal brovndel : boolean;
  signal rroeorii : real;
  signal hvqfbptlh : time;
  signal jlpvjqtguq : bit;
  signal ylasy : boolean;
  signal wontycl : real;
  signal jsghpe : time;
  signal yhsmsru : bit;
  signal kxhip : boolean;
  signal ar : real;
  signal illljyy : bit;
  signal lld : boolean;
  signal rvsaztysfl : real;
  signal wrwzzr : time;
begin
  qrq : entity work.qeof
    port map (ulloqurnl => wrwzzr, yeemadtpg => rvsaztysfl, qwugpopync => lld, nkhgk => illljyy);
  m : entity work.qeof
    port map (ulloqurnl => wrwzzr, yeemadtpg => ar, qwugpopync => kxhip, nkhgk => yhsmsru);
  cwaphqar : entity work.qeof
    port map (ulloqurnl => jsghpe, yeemadtpg => wontycl, qwugpopync => ylasy, nkhgk => jlpvjqtguq);
  rqbypoooha : entity work.qeof
    port map (ulloqurnl => hvqfbptlh, yeemadtpg => rroeorii, qwugpopync => brovndel, nkhgk => g);
  
  -- Single-driven assignments
  y <= (8#3_1#, 2#0_0#);
  hvqfbptlh <= wrwzzr;
  jsghpe <= wrwzzr;
  wrwzzr <= 1300 fs;
end l;



-- Seed after: 15536905517652845426,511364357853360275
