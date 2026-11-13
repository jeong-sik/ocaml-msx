# ocaml-msx 로드맵

MSX2+ 를 OCaml 로 에뮬레이트한다. 최종 목표는 두 개다:

1. 사람이 masc TUI 탭 하나로 게임을 플레이한다.
2. 같은 기계에 Keeper 가 도구로 입력을 넣어 같이 플레이한다.

## 목표 게임 (완료 판정의 기준)

| 게임 | 미디어 | 요구 |
|---|---|---|
| 몽환전사 바리스 | ROM (재믹스 유통) | MSX2, V9938, command engine, 라인 인터럽트 |
| 몽환전사 바리스 2 | 디스크 | + FDC(.DSK) |
| 이스 II | 디스크 2장 | + FM-PAC(YM2413), PAC SRAM 세이브 |
| 룬 마스터 II | 디스크, MSX2, 턴제 보드 RPG | Keeper 동시 플레이 타깃 (P5) |

게임 덤프는 사용자 소관. 이 저장소는 C-BIOS(2-clause BSD)만 실어 배포한다.

## 사다리

| 단계 | 내용 | 완료 기준 |
|---|---|---|
| P0 | Z80 코어 + zexall/zexdoc 하네스 | **완료 (2026-09-07)** — zexall·zexdoc 전 그룹 PASS, 5.76B 명령 / 46.7G T-state. 하네스: `bin/zex.exe`, 단위 벡터 `bin/vectors.exe`, C 오라클 차등 `bin/trace.exe` + `test/zex/diff_driver.c` |
| P1 | MSX1 — V9938 의 TMS 호환 모드, PPI, RAM, C-BIOS | **완료 (2026-09-07)** — XSpelunker(32KB 카트리지, SCREEN 2 + 스프라이트 모드 1) 가 부트 로고 → 타이틀 → LEVEL 1-1 → 게임플레이. 하네스 `bin/boot.exe --roms roms/cbios --cart <rom> --tap-space 340,420`. 색 0 투명(R#7 배경색, TP)·화면 끔 테두리색 — `test/backdrop_test.ml`. 문자 모드의 R#23 세로 스크롤·212줄, MULTICOLOR(SCREEN 3), TEXT2(80열)·R#12/R#13 깜빡임, 비트맵 R#2 페이지 마스크·EO/깜빡임 페이지 교대, 세이브 형식 v10 — `test/charmode_test.ml`. R#18 화면 위치 조정(가로는 줄마다, 세로는 줄 카운터 리셋에서 래치), 세이브 형식 v11 — `test/adjust_test.ml`. 화면 모드·화면 끔·스프라이트 끔 비트는 다음 줄 시작(그래픽 모드 39사이클, 텍스트 43사이클)에서 적용(openMSX syncAtNextLine, 측정 ROM 으로 openMSX 와 대조), 세이브 형식 v12 — `test/line_start_test.ml`. 남은 틈: 빠른 깜빡임(R#1 bit 2). 조이스틱 방향은 방향키가 커서(행 8)와 joy1 비트(PSG R#14)를 함께 구동해 닫힘 |
| P2 | MSX2 — command engine, 라인 인터럽트, 128KB VRAM, 팔레트, 스프라이트 모드 2 | 바리스 1·2 (사람이 TUI 로). 진행: 스프라이트 모드 1·2 (라인당 4/8개, 5S/9S, 충돌·S#3-S#6, CC/IC/EC, SCREEN8 고정 팔레트) — `test/sprite_test.ml`, `test/sprite2_test.ml`. 라인 래스터: 빔이 지나는 순간의 상태로 라인을 그리고 VBlank 가 프레임을 확정, 라인 인터럽트는 (R#19 - R#23) — `test/raster_test.ml`. 명령 엔진: POINT/SRCH/LINE/LMCM/STOP, S#2 BD·S#7·S#8/S#9, SCREEN5-8 에서만 실행 — `test/cmd_test.ml`. 프레임 타이밍: S#2 VR·HR·EO, R#9 PAL 313줄, 테두리 줄 라인 인터럽트 — `test/status2_test.ml`. 프레임 위상: VDP 는 frame start 에서 켜지고(openMSX VDP::reset), 줄 카운터는 frame start 뒤 8 + ((R#18 >> 4) ^ 7)줄에서 리셋돼 그때의 R#9 에 따라 17/27/44/54줄 뒤 표시 줄 0 이 된다(openMSX master, FS-A1GT·FS-A1ST 실측). 192/212줄 전환과 R#18 세로 조정은 화면 시작만 옮기고 frame start 는 262/313줄 격자를 지킨다, 세이브 형식 v14 — `test/frame_phase_test.ml`. MSXgl 76개의 10초 VRAM 이 openMSX 와 32개 같다(이전 25개). 명령 실행 시간: 명령은 VRAM 접근 슬롯(openMSX 21.0 의 V9938 실측 표: 화면 끔·스프라이트 끔·스프라이트 켬)을 하나씩 쓰며 진행하고 CE 를 그동안 유지한다, CPU 의 VRAM 접근이 슬롯을 뺏는다, 세이브 형식 v13 — `test/cmd_timing_test.ml`. 측정 ROM 33개(명령 11종 × 화면 3종)의 CE 폴링 횟수가 openMSX 와 29개 같고 4개는 1회 차이. VBlank: S#0 F 와 S#2 VR 은 마지막 표시 줄 아래 줄의 48사이클째에 선다(openMSX VScan). C-BIOS 의 JIFFY 쓰기 시각이 openMSX 와 5사이클 안에서 같은 프레임이 s_math 186/228, s_game 204/228, template 212/228 이다(이전 0-1). 남은 것: LMMC/HMMC/LMCM 전송의 시간 |
| P3 | 디스크·사운드 — FDC(.DSK), PSG, YM2413, PAC | 이스 II (FM 음악, 세이브). 진행: PSG(AY-3-8910) 톤·노이즈·엔벨로프를 44.1 kHz 모노 PCM 으로 섞어 `Msx.audio` 로 낸다, 세이브 형식 v8 — `test/psg_test.ml`. Konami SCC 카트의 SCC 5채널을 같은 믹서로 섞는다(openMSX 기본 볼륨 비), 세이브 형식 v9 — `test/scc_test.ml`. 남은 것: YM2413(FM-PAC), PAC SRAM, SCC-I(SCC+), 클라이언트의 소리 재생 |
| P4 | MSX2+ — V9958 (YJK/YAE 등) | MSX2+ 타이틀 확정 시 지정 |
| P5 | 동시 플레이 — masc 도구 표면, 멀티 클라이언트 키 집합 | 사람과 Keeper 가 같은 세션에서 룬 마스터 II |

통합 순서: 사람이 TUI 로 플레이(P2) → Keeper 참여(P5).

## 코어 계약 (lib/msx.mli)

- 순수: 시간·파일·난수·터미널을 코어가 스스로 읽지 않는다. `step` 호출이 시간을 진행한다.
- 결정론: 같은 상태 + 같은 입력 = 같은 프레임. `serialize`/`restore` 로 구간 재현.
- 프레임은 네이티브 해상도 RGB. 다운샘플과 렌더링은 클라이언트 몫(masc 모자이크/키티).
- 키는 논리 키 주입. `set_key` 는 자리가 있으면 true, 없으면(표 밖 글자, F6 이상) 아무것도 바꾸지 않고 false. 누가 눌렀는지(TUI 사람/keeper)는 코어 밖에서 집합으로 관리한다. 키보드 매트릭스의 정본은 openMSX `unicodemap.int`, 조이스틱 포트(PSG R#14)의 빈 값은 0x3F.
- 실시간 60fps 는 클라이언트의 선택이고, 코어의 기본은 턴제 스텝이다.

## 관전과 재생

- **재생**: `bin/replay.exe --roms DIR --cart ROM --ledger ledger.jsonl --out-dir DIR` 이 masc 가
  기록한 입력 원장(`frame, who, key, edge`)을 결정론적으로 다시 돌려 프레임을 PPM 으로 덤프한다.
  같은 카트리지 + 같은 원장 = 같은 화면(코어는 시계·난수를 안 읽는다). 사후 관전·GIF 용.
- **세이브 스테이트**: v3 봉투가 WD2793 드라이브 상태(cmd/track/sector/data/side/motor/step_out/busy/drq/intr/buf/pos/side_reg/drive_reg/intr_fired)를 저장한다(ocaml-msx #46). 복원은 냉시동으로 다시 출발하지 않는다.
- **라이브 관전**: masc TUI `&` 화면이 서버 프레임을 받아 그린다 (RFC-0439 §3.7, masc PR).

## 클라이언트

- `bin/msx_demo` — 터미널 half-block 데모. 지금은 스텁 패턴.
- masc TUI 스펙테이터 탭 — opam path pin 으로 소비. `image_mosaic`/키티 그래이픽으로 중계. ROM 은 `MSX_ROMS`(C-BIOS 디렉터리)·`MSX_CART`(카트리지 한 장) 환경변수.
- keeper 도구 — masc 어휘 라우팅 규칙(variant 라우팅과 `all` 광고 일치)을 따라 등록.

## 원칙

- 하네스 먼저: zexall(P0), 프레임 회귀 덤프(P1~), savestate 왕복 테스트. 렌더·입력 계약은 `dune test` 의 sprite/keyboard/g2 테스트가 포트 경로로 판정한다.
- savestate 포맷은 버전 태그 + 하드컷. 옛 포맷용 변환기·리더를 만들지 않는다.
- 오디오: 코어는 샘플 버퍼만 채운다. 스피커로 내보내는 건 클라이언트 몫.

## 참조

- [V9938 Application Manual (grauw)](https://map.grauw.nl/resources/video/v9938/v9938.xhtml)
- [msx_rs — V9938 command engine 참고 구현 (Rust)](https://github.com/joskwanten/msx_rs)
- [openMSX VDP VRAM 타이밍 문서](https://openmsx.org/vdp-vram-timing/vdp-timing.html)
- [antirez — zexall 통과하는 Z80 (1,200줄 C)](https://antirez.com/news/160), [superzazu/z80](https://github.com/superzazu/z80)
- [C-BIOS](https://cbios.sourceforge.net/)
