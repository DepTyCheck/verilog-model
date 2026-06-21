-- Seed: 2546845936565565618,3687118713772291287

entity ggqjlgk is
  port (lwqjsu : linkage real_vector(4 to 3); aqhfus : out bit; zb : out boolean);
end ggqjlgk;

architecture xsuzxsfry of ggqjlgk is
  
begin
  -- Single-driven assignments
  aqhfus <= '1';
  zb <= FALSE;
end xsuzxsfry;

entity qqjl is
  port (sh : buffer integer);
end qqjl;

architecture tpzwlmh of qqjl is
  signal jlgn : boolean;
  signal i : bit;
  signal ocd : real_vector(4 to 3);
  signal axbofkdghg : boolean;
  signal rzhfgcpx : bit;
  signal atcgrexg : real_vector(4 to 3);
begin
  jwq : entity work.ggqjlgk
    port map (lwqjsu => atcgrexg, aqhfus => rzhfgcpx, zb => axbofkdghg);
  sqc : entity work.ggqjlgk
    port map (lwqjsu => ocd, aqhfus => i, zb => jlgn);
  
  -- Single-driven assignments
  sh <= 8#70150#;
end tpzwlmh;



-- Seed after: 6784148442854643016,3687118713772291287
