-- Seed: 18000901189446168473,3687118713772291287

entity awmdur is
  port (gfdo : inout bit; usin : in integer; vfip : inout real; decpjn : in time);
end awmdur;

architecture x of awmdur is
  
begin
  -- Single-driven assignments
  vfip <= 0113.2_4;
  gfdo <= '1';
end x;

entity px is
  port (wzok : buffer string(4 downto 2); acuveftvg : in integer);
end px;

architecture rs of px is
  signal qmk : time;
  signal iixhfewd : real;
  signal zfdbez : integer;
  signal guqqh : bit;
  signal qfbmgdr : time;
  signal iwvvgxjqt : real;
  signal quzieehme : bit;
  signal hfot : time;
  signal gbbxxiy : real;
  signal fy : integer;
  signal backyojv : bit;
  signal fitqh : time;
  signal atf : real;
  signal liwsj : integer;
  signal jopo : bit;
begin
  ecdulgaeyi : entity work.awmdur
    port map (gfdo => jopo, usin => liwsj, vfip => atf, decpjn => fitqh);
  ivipxvez : entity work.awmdur
    port map (gfdo => backyojv, usin => fy, vfip => gbbxxiy, decpjn => hfot);
  pvhms : entity work.awmdur
    port map (gfdo => quzieehme, usin => fy, vfip => iwvvgxjqt, decpjn => qfbmgdr);
  rscn : entity work.awmdur
    port map (gfdo => guqqh, usin => zfdbez, vfip => iixhfewd, decpjn => qmk);
  
  -- Single-driven assignments
  hfot <= 2#0_1_1_0.00101# ps;
  wzok <= ('t', 'h', 'p');
  fitqh <= 16#E5765.2DE# ns;
  liwsj <= 2_1_4_3_4;
  fy <= 8#4_2_2_6#;
end rs;



-- Seed after: 14432794970062164862,3687118713772291287
