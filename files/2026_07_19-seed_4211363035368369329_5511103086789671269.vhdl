-- Seed: 4211363035368369329,5511103086789671269

entity jwwdxhb is
  port (zren : in time; whrjh : buffer integer; hpgxq : out bit);
end jwwdxhb;

architecture t of jwwdxhb is
  
begin
  -- Single-driven assignments
  hpgxq <= hpgxq;
  whrjh <= whrjh;
end t;

entity zh is
  port (rmqrmzv : in integer);
end zh;

architecture nzozb of zh is
  signal swcyuqut : bit;
  signal cy : integer;
  signal tmdswssv : time;
  signal vfofkuvvh : bit;
  signal wu : integer;
  signal pczdzkztei : time;
begin
  fkd : entity work.jwwdxhb
    port map (zren => pczdzkztei, whrjh => wu, hpgxq => vfofkuvvh);
  vnqxzpgfbv : entity work.jwwdxhb
    port map (zren => tmdswssv, whrjh => cy, hpgxq => swcyuqut);
  
  -- Single-driven assignments
  tmdswssv <= pczdzkztei;
  pczdzkztei <= 1 sec;
end nzozb;



-- Seed after: 13085344205751122965,5511103086789671269
